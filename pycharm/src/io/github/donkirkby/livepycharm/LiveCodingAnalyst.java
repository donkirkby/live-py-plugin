package io.github.donkirkby.livepycharm;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.RunManagerEx;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParamsGroup;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.*;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.util.ProgressIndicatorBase;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Alarm;
import com.jetbrains.python.run.CommandLinePatcher;
import com.jetbrains.python.run.PythonCommandLineState;
import io.github.donkirkby.livecanvas.CanvasCommand;
import io.github.donkirkby.livecanvas.CanvasReader;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class LiveCodingAnalyst implements DocumentListener {
    public interface CanvasPainter {
        void setCommands(List<CanvasCommand> commands);

        Rectangle getBounds();
    }

    private static Logger log = Logger.getInstance(LiveCodingAnalyst.class);
    private final VirtualFile mainFile;
    private final Document displayDocument;
    private boolean isRunning;
    private static ExecutorService pool = Executors.newCachedThreadPool();
    private PythonCommandLineState commandLineState;
    private CommandLinePatcher commandLinePatcher;
    private Alarm alarm = new Alarm();
    private ProgressIndicator progressIndicator = new ProgressIndicatorBase(true);
    private CanvasPainter canvasPainter;
    private boolean isPassing;

    LiveCodingAnalyst(VirtualFile mainFile,
                      Document displayDocument,
                      Disposable parent,
                      CanvasPainter painter) {
        this.mainFile = mainFile;
        this.displayDocument = displayDocument;
        this.canvasPainter = painter;
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        Document document = documentManager.getDocument(mainFile);
        if (document != null) {
            document.addDocumentListener(this, parent);
        }
    }

    Document getDisplayDocument() {
        return displayDocument;
    }

    boolean isPassing() {
        return isPassing;
    }

    /**
     * Try to start a new analysis job.
     * @param project the project for the current action
     * @param dataContext the data context for the current action
     * @return true if the analysis successfully started
     */
    boolean start(@Nullable Project project, @NotNull DataContext dataContext) {
        if (project == null || project.isDisposed()) {
            return false;
        }

        RunnerAndConfigurationSettings configuration = RunManagerEx.getInstanceEx(project).getSelectedConfiguration();
        if (configuration == null) {
            return false;
        }
        DefaultRunExecutor executor = new DefaultRunExecutor();
        ExecutionEnvironmentBuilder builder = ExecutionEnvironmentBuilder.createOrNull(executor, configuration);
        if (builder == null) {
            return false;
        }
        ExecutionEnvironment environment = builder.activeTarget().dataContext(dataContext).build();
        try {
            RunProfileState state = environment.getState();
            if (! (state instanceof PythonCommandLineState)) {
                return false;
            }
            commandLineState = (PythonCommandLineState) state;
        } catch (com.intellij.execution.ExecutionException e1) {
            return false;
        }
        File plugins = new File(PathManager.getPluginsPath());
        File livePyPath = new File(plugins, "livepy");
        File pythonPath;
        if (livePyPath.isDirectory()) {
            pythonPath = new File(livePyPath, "classes");
        } else {
            pythonPath = new File(plugins, "livepy.jar");
        }
        commandLinePatcher = commandLine -> {
            Map<String, String> environment1 = commandLine.getEnvironment();
            ParamsGroup paramsGroup = commandLine.getParametersList().getParamsGroup(
                    PythonCommandLineState.GROUP_MODULE);
            if (paramsGroup == null || paramsGroup.getParameters().isEmpty()) {
                paramsGroup = commandLine.getParametersList().getParamsGroup(
                        PythonCommandLineState.GROUP_SCRIPT);
            }
            if (paramsGroup == null) {
                return;
            }
            String driverPath = paramsGroup.getParametersList().get(0);
            String oldPythonPath = environment1.get("PYTHONPATH");
            String newPythonPath;
            if (oldPythonPath == null || oldPythonPath.length() == 0) {
                newPythonPath = pythonPath.getAbsolutePath();
            }
            else {
                newPythonPath = pythonPath.getAbsolutePath() +
                        File.pathSeparator + oldPythonPath;
            }
            environment1.put("PYTHONPATH", newPythonPath);
            String modulePath = mainFile.getCanonicalPath();
            if (modulePath == null) {
                modulePath = mainFile.getPath();
            }
            boolean hasDriver = ! driverPath.equals(modulePath);
            int i = 0;
            Rectangle bounds = canvasPainter.getBounds();
            paramsGroup.addParameterAt(i++, "-m");
            paramsGroup.addParameterAt(i++, "code_tracer");
            String moduleName = hasDriver
                    ? getModuleName(new File(mainFile.getPath()), oldPythonPath)
                    : "__live_coding__";
            paramsGroup.addParameterAt(i++, "--filename");
            paramsGroup.addParameterAt(i++, modulePath);
            String badDriverMessage = buildBadDriverMessage(configuration, moduleName);
            paramsGroup.addParameterAt(i++, "--bad_driver");
            paramsGroup.addParameterAt(i++, badDriverMessage);
            paramsGroup.addParameterAt(i++, "--canvas");
            paramsGroup.addParameterAt(i++, "--width");
            paramsGroup.addParameterAt(i++, Integer.toString(bounds.width));
            paramsGroup.addParameterAt(i++, "--height");
            paramsGroup.addParameterAt(i++, Integer.toString(bounds.height));
            paramsGroup.addParameterAt(i++, "-"); // source code from stdin
            paramsGroup.addParameterAt(i, moduleName);
        };
        isRunning = true;
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        Document document = documentManager.getDocument(mainFile);
        if (document != null) {
            schedule(document);
        }
        return true;
    }

    private String buildBadDriverMessage(
            RunnerAndConfigurationSettings runConfiguration,
            String moduleName) {
        return String.format(
                            "%s doesn't call the %s module." +
                                    " Try a different run configuration.",
                            runConfiguration.getName(),
                            moduleName);
    }

    void stop() {
        isRunning = false;
    }

    @Override
    public void documentChanged(DocumentEvent e) {
        if (isRunning) {
            FileDocumentManager documentManager =
                    FileDocumentManager.getInstance();
            Document[] unsavedDocuments = documentManager.getUnsavedDocuments();
            Document eventDocument = e.getDocument();
            for (Document unsavedDocument: unsavedDocuments) {
                if (unsavedDocument != eventDocument) {
                    documentManager.saveDocument(unsavedDocument);
                }
            }
            schedule(eventDocument);
        }
    }

    private synchronized void analyse(Document document) {
        progressIndicator.start();
        try {
            String sourceCode = document.getText();
            String display = null;
            try {
                CapturingProcessHandler processHandler = startProcess(sourceCode);
                ProcessOutput processOutput =
                        processHandler.runProcessWithProgressIndicator(progressIndicator);
                display = processOutput.getStdout();
                String stderr = processOutput.getStderr();
                isPassing = processOutput.getExitCode() == 0;
                if (stderr.length() > 0) {
                    log.error(stderr);
                }
            } catch (ExecutionException | IOException ex) {
                log.error("Report failed.", ex);
            }
            if (display == null || progressIndicator.isCanceled()) {
                return;
            }
            final String finalDisplay = display;
            ApplicationManager.getApplication().invokeLater(() -> displayResult(finalDisplay));
        } finally {
            progressIndicator.stop();
        }
    }

    private void schedule(Document document) {
        if (progressIndicator.isRunning()) {
            progressIndicator.cancel();
        }
        alarm.cancelAllRequests();
        alarm.addRequest(() -> pool.submit(() -> analyse(document)), 300);
    }

    private void displayResult(String display) {
        String canvasStart = String.format("start_canvas%n");
        if ( ! display.startsWith(canvasStart)) {
            canvasPainter.setCommands(null);
        }
        else {
            BufferedReader reader = new BufferedReader(new StringReader(display));
            try {
                reader.readLine();  // Skip first line.
                CanvasReader canvasReader = new CanvasReader(reader);
                canvasPainter.setCommands(canvasReader.readCommands());

                StringWriter writer = new StringWriter();
                PrintWriter printer = new PrintWriter(writer);
                while (true) {
                    String line = reader.readLine();
                    if (line == null) {
                        break;
                    }
                    printer.println(line);
                }
                display = writer.toString();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        final String finalDisplay = display;
        ApplicationManager.getApplication().runWriteAction(
                () -> displayDocument.setText(finalDisplay));
    }

    private CapturingProcessHandler startProcess(String sourceCode) throws ExecutionException, IOException {
        final GeneralCommandLine commandLine =
                ApplicationManager.getApplication().runReadAction(
                        (Computable<GeneralCommandLine>) () ->
                                commandLineState.generateCommandLine(
                                        new CommandLinePatcher[]{commandLinePatcher}));
        final CapturingProcessHandler processHandler = new CapturingProcessHandler(commandLine);
        try {
            byte[] stdin = sourceCode.getBytes();
            final OutputStream processInput = processHandler.getProcessInput();
            assert processInput != null;
            processInput.write(stdin);
            processInput.close();
            return processHandler;
        } catch (IOException e) {
            processHandler.destroyProcess();
            throw e;
        }
    }

    private String getModuleName(
            File file,
            String pythonPath) {
        String[] paths = pythonPath.split(File.pathSeparator);
        Path absolutePath = file.toPath();
        Path shortestPath = absolutePath;
        for (String path : paths) {
            Path filePath = Paths.get(path).relativize(absolutePath);
            if (filePath.getNameCount() < shortestPath.getNameCount()) {
                shortestPath = filePath;
            }
        }
        StringBuilder moduleName = new StringBuilder();
        for (java.nio.file.Path component : shortestPath) {
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
}
