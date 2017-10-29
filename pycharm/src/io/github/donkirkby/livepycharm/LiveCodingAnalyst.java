package io.github.donkirkby.livepycharm;

import com.google.common.collect.Lists;
import com.google.gson.Gson;
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
import org.jdom.Attribute;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;

public class LiveCodingAnalyst extends DocumentAdapter {
    private static Logger log = Logger.getInstance(LiveCodingAnalyst.class);
    private static ExecutorService pool = Executors.newCachedThreadPool();
    private final VirtualFile mainFile;
    private final Document displayDocument;
    private boolean isRunning;
    private boolean isBusy;
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
        // Python or Unittests
        String factoryName = runConfiguration.getFactory().getName();
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
                String moduleName = getModuleName(
                        new File(mainFile.getPath()),
                        workingDir);
                String badDriverMessage = buildBadDriverMessage(runConfiguration, moduleName);
                processArguments.add("--bad_driver");
                processArguments.add(badDriverMessage);
                processArguments.add("-"); // source code from stdin
                processArguments.add(moduleName);
                processArguments.add(driverPath);
                List<String> driverParameters = CommandLineUtil.toCommandLine(
                        Lists.newArrayList(
                                "echo",
                                pythonConfiguration.getScriptParameters()));
                driverParameters.remove(0);
                processArguments.addAll(driverParameters);
            }
        } else if ("Unittests".equals(factoryName)) {
            Gson gson = new Gson();
            Element element = new Element("dummy");
            runConfiguration.writeExternal(element);
            Map<String, String> options = new HashMap<>();
            for (Element option : element.getChildren("option")) {
                Attribute nameAttribute = option.getAttribute("name");
                Attribute valueAttribute = option.getAttribute("value");
                if (nameAttribute != null && valueAttribute != null) {
                    options.put(
                            nameAttribute.getValue(),
                            valueAttribute.getValue());
                }
            }
            // TODO: _new_targetType, _new_pattern, _new_additionalArguments
            String target = gson.fromJson(options.get("_new_target"), String.class);

            workingDir = options.get("WORKING_DIRECTORY");
            String moduleName = getModuleName(
                    new File(mainFile.getPath()),
                    workingDir);
            String badDriverMessage = buildBadDriverMessage(runConfiguration, moduleName);
            processArguments = Lists.newArrayList(
                    options.get("SDK_HOME"),
                    "-m",
                    "code_tracer");
            processArguments.add("--bad_driver");
            processArguments.add(badDriverMessage);
            processArguments.add("-"); // source code from stdin
            processArguments.add(moduleName);
            processArguments.add("-m");
            processArguments.add("unittest");
            processArguments.add(target);
        } else {
            return;
        }
        isRunning = true;
        finish();
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        Document document = documentManager.getDocument(mainFile);
        if (document != null) {
            schedule(document, false);
        }
    }

    private String buildBadDriverMessage(RunConfiguration runConfiguration, String moduleName) {
        return String.format(
                            "%s doesn't call the %s module." +
                                    " Try a different run configuration.",
                            runConfiguration.getName(),
                            moduleName);
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
        public Continuation performInReadAction(
                @NotNull ProgressIndicator indicator)
                throws ProcessCanceledException {
            String sourceCode = document.getText();
            String display = null;
            try {
                Process process = startProcess(sourceCode);
                try {
                    Future<String> reader = pool.submit(
                            () -> readOutput(process));
                    display = await(reader, indicator);
                } catch (Exception ex) {
                    process.destroy();
                    throw ex;
                }
            } catch (InterruptedException | ExecutionException | IOException ex) {
                log.error("Report failed.", ex);
            }
            finish();
            if (display == null) {
                return null;
            }
            final String finalDisplay = display;
            return new Continuation(() -> displayResult(finalDisplay));
        }

        private <T> T await(Future<T> future, ProgressIndicator indicator)
                throws ExecutionException, InterruptedException {
            while (true) {
                try {
                    return future.get(50, TimeUnit.MILLISECONDS);
                } catch (TimeoutException e) {
                    try {
                        indicator.checkCanceled();
                    } catch (ProcessCanceledException ex) {
                        final boolean mayInterrupt = true;
                        future.cancel(mayInterrupt);
                        throw ex;
                    }
                }
            }
        }

        @Override
        public void onCanceled(@NotNull ProgressIndicator indicator) {
            schedule(document, true);
        }
    }

    @Override
    public void documentChanged(DocumentEvent e) {
        if (isRunning) {
            schedule(e.getDocument(), false);
        }
    }

    private synchronized void schedule(Document document, boolean isRestart) {
        if (isBusy && ! isRestart) {
            return;
        }
        isBusy = true;
        ProgressIndicatorUtils.scheduleWithWriteActionPriority(new AnalysisTask(document));
    }

    private synchronized void finish() {
        isBusy = false;
    }

    private void displayResult(String display) {
        ApplicationManager.getApplication().runWriteAction(() -> displayDocument.setText(display));
    }

    @NotNull
    private Process startProcess(String sourceCode) throws IOException {
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
        Process process = processBuilder.start();
        OutputStream outputStream = process.getOutputStream();
        outputStream.write(sourceCode.getBytes());
        outputStream.close();
        if (Thread.interrupted()) {
            process.destroy();
        }
        return process;
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
    private String readOutput(Process process) throws IOException, InterruptedException {
        InputStream inputStream = process.getInputStream();
        BufferedReader reader =
                new BufferedReader(new InputStreamReader(inputStream));
        StringBuilder builder = new StringBuilder();
        String line;
        while (null != (line = reader.readLine())) {
            builder.append(line);
            builder.append("\n");
        }
        process.waitFor();
        return builder.toString();
    }
}
