package io.github.donkirkby.livepycharm;

import com.google.common.collect.Lists;
import com.intellij.execution.CommandLineUtil;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.event.DocumentAdapter;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.util.ProgressIndicatorUtils;
import com.intellij.openapi.progress.util.ReadTask;
import com.intellij.openapi.vfs.VirtualFile;
import com.jetbrains.python.run.PythonRunConfiguration;
import com.jetbrains.python.testing.universalTests.PyUniversalUnitTestConfiguration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class LiveCodingAnalyst extends DocumentAdapter {
    private static Logger log = Logger.getInstance(LiveCodingAnalyst.class);
    private static ExecutorService pool = Executors.newCachedThreadPool();
    private final VirtualFile mainFile;
    private final Document displayDocument;
    private boolean isRunning;
    private String prevSourceCode;  // last source code analysed without being cancelled
    private ArrayList<String> processArguments;
    private String workingDir;

    LiveCodingAnalyst(VirtualFile mainFile, Document displayDocument, Disposable parent) {
        this.mainFile = mainFile;
        this.displayDocument = displayDocument;
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        Document document = documentManager.getDocument(mainFile);
        if (document != null) {
            document.addDocumentListener(this, parent);
        }
    }

    void start(RunConfiguration runConfiguration) {
        if (runConfiguration instanceof PythonRunConfiguration) {
            PythonRunConfiguration pythonConfiguration =
                    (PythonRunConfiguration) runConfiguration;
            workingDir = pythonConfiguration.getWorkingDirectory();
            processArguments = Lists.newArrayList(
                    pythonConfiguration.getSdkHome(),
                    "-m",
                    "code_tracer");
            String driverPath;
            driverPath = pythonConfiguration.getScriptName();
            String modulePath = mainFile.getCanonicalPath();
            if (!driverPath.equals(modulePath)) {
                processArguments.add("-"); // source code from stdin
                String moduleName = getModuleName(
                        new File(mainFile.getPath()),
                        workingDir);
                processArguments.add(moduleName);
                processArguments.add(driverPath);
                List<String> driverParameters = CommandLineUtil.toCommandLine(
                        Lists.newArrayList(
                                "echo",
                                pythonConfiguration.getScriptParameters()));
                driverParameters.remove(0);
                processArguments.addAll(driverParameters);
            }
        } else if (runConfiguration instanceof PyUniversalUnitTestConfiguration) {
            PyUniversalUnitTestConfiguration unitTestConfiguration =
                    (PyUniversalUnitTestConfiguration) runConfiguration;
            workingDir = unitTestConfiguration.getWorkingDirectory();
            processArguments = Lists.newArrayList(
                    unitTestConfiguration.getSdkHome(),
                    "-m",
                    "code_tracer");
            processArguments.add("-"); // source code from stdin
            String moduleName = getModuleName(
                    new File(mainFile.getPath()),
                    workingDir);
            processArguments.add(moduleName);
            processArguments.add("-m");
            processArguments.add("unittest");
            processArguments.add(unitTestConfiguration.getTarget().getTarget());
        } else {
            return;
        }
        prevSourceCode = null;
        isRunning = true;
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        Document document = documentManager.getDocument(mainFile);
        if (document != null) {
            schedule(document);
        }
    }

    void stop() {
        isRunning = false;
    }

    private class AnalysisTask extends ReadTask {
        private Document document;

        AnalysisTask(Document document) {
            this.document = document;
        }

        @Nullable
        @Override
        public Continuation performInReadAction(@NotNull ProgressIndicator indicator) throws ProcessCanceledException {
            String sourceCode = document.getText();
            String display = analyseDocument(sourceCode, indicator);
            if (display == null) {
                return null;
            }
            return new Continuation(() -> displayResult(display));
        }

        @Override
        public void onCanceled(@NotNull ProgressIndicator indicator) {
            schedule(document);
        }
    }

    @Override
    public void documentChanged(DocumentEvent e) {
        if (isRunning) {
            schedule(e.getDocument());
        }
    }

    private void schedule(Document document) {
        ProgressIndicatorUtils.scheduleWithWriteActionPriority(new AnalysisTask(document));
    }

    private void displayResult(String display) {
        ApplicationManager.getApplication().runWriteAction(() -> displayDocument.setText(display));
    }

    @Nullable
    private synchronized String analyseDocument(String source, ProgressIndicator indicator) {
        if (source.equals(prevSourceCode)) {
            return null;
        }
        String output = "";
        File plugins = new File(PathManager.getPluginsPath());
        File livePyPath = new File(plugins, "livepy");
        File pythonPath;
        if (livePyPath.isDirectory()) {
            pythonPath = new File(livePyPath, "classes");
        } else {
            pythonPath = new File(plugins, "livepy.jar");
        }
        ProcessBuilder processBuilder = new ProcessBuilder(
                processArguments)
                .directory(new File(workingDir))
                .redirectInput(ProcessBuilder.Redirect.PIPE)
                .redirectOutput(ProcessBuilder.Redirect.PIPE)
                .redirectErrorStream(true);
        processBuilder.environment().put("PYTHONPATH", pythonPath.getAbsolutePath());
        try {
            Process process = processBuilder.start();
            OutputStream outputStream = process.getOutputStream();
            outputStream.write(source.getBytes());
            outputStream.close();
            Future<String> reader = pool.submit(() -> readOutput(process));
            boolean isWaiting = true;
            while (isWaiting) {
                try {
                    output = reader.get(50, TimeUnit.MILLISECONDS);
                    isWaiting = false;
                } catch (TimeoutException e) {
                    try {
                        indicator.checkCanceled();
                    } catch (ProcessCanceledException ex) {
                        process.destroy();
                        throw ex;
                    }
                }
            }
        } catch (IOException | InterruptedException | ExecutionException ex) {
            log.error("Report failed.", ex);
        }

        prevSourceCode = source;
        return output;
    }

    private String getModuleName(
            File file,
            String pythonPath) {
        Path pythonPath2 = Paths.get(pythonPath);
        Path filePath2 = file.toPath();
        Path filePath = pythonPath2.relativize(filePath2);
        StringBuilder moduleName = new StringBuilder();
        for (java.nio.file.Path component : filePath) {
            if (moduleName.length() > 0) {
                moduleName.append(".");
            }
            moduleName.append(component.getFileName());
        }
        if (moduleName.toString().endsWith(".py")) {
            return moduleName.substring(0, moduleName.length() - 3);
        }
        return moduleName.toString();
    }

    @NotNull
    private String readOutput(Process process) throws IOException {
        InputStream inputStream = process.getInputStream();
        BufferedReader reader =
                new BufferedReader(new InputStreamReader(inputStream));
        StringBuilder builder = new StringBuilder();
        String line;
        while (null != (line = reader.readLine())) {
            builder.append(line);
            builder.append("\n");
        }
        return builder.toString();
    }
}
