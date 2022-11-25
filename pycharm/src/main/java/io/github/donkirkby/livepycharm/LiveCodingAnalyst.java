package io.github.donkirkby.livepycharm;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.RunManagerEx;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParamsGroup;
import com.intellij.execution.configurations.RunConfiguration;
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
import com.intellij.openapi.editor.*;
import com.intellij.openapi.editor.actions.AbstractToggleUseSoftWrapsAction;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.editor.impl.EditorImpl;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.util.ProgressIndicatorBase;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Alarm;
import com.jetbrains.python.run.CommandLinePatcher;
import com.jetbrains.python.run.PythonCommandLineState;
import com.jetbrains.python.run.PythonRunConfiguration;
import io.github.donkirkby.livecanvas.CanvasCommand;
import io.github.donkirkby.livecanvas.CanvasReader;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LiveCodingAnalyst implements DocumentListener {
    public interface CanvasPainter {
        void setCommands(List<CanvasCommand> commands);

        Rectangle getBounds();
    }

    private static final Logger log = Logger.getInstance(LiveCodingAnalyst.class);
    private static final Pattern GOAL_PATTERN =
            Pattern.compile(":lesson goal file:\\s*(\\S*)");
    private static final String CANVAS_START = "start_canvas";
    private final VirtualFile mainFile;
    private final Document displayDocument;
    private boolean isRunning;
    private static final ExecutorService pool = Executors.newCachedThreadPool();
    private PythonCommandLineState commandLineState;
    private CommandLinePatcher commandLinePatcher;
    private final Alarm alarm = new Alarm();
    private final ProgressIndicator progressIndicator = new ProgressIndicatorBase(true);
    private final CanvasPainter canvasPainter;
    private String goalFile;
    private Rectangle goalBounds;
    private CanvasCommand goalImageCommand;
    private boolean isPassing;
    private boolean isDisplayUpdating = true;
    private String activeConfigurationId;

    LiveCodingAnalyst(VirtualFile mainFile,
                      Document displayDocument,
                      Disposable parent,
                      CanvasPainter painter) {
        this.mainFile = mainFile;
        this.displayDocument = displayDocument;
        this.canvasPainter = painter;
        Document document = getMainDocument();
        if (document != null) {
            document.addDocumentListener(this, parent);
        }
        displayDocument.addDocumentListener(new DocumentListener() {
            @Override
            public void documentChanged(@NotNull DocumentEvent event) {
                synchronizeInlays();
            }
        });
    }

    Document getDisplayDocument() {
        return displayDocument;
    }

    boolean isPassing() {
        return isPassing;
    }

    boolean isDisplayUpdating() {
        return isDisplayUpdating;
    }

    boolean isRunningSelectedConfiguration(@Nullable Project project) {
        if (!this.isRunning) {
            return false;
        }

        if (project == null || project.isDisposed()) {
            return false;
        }

        RunnerAndConfigurationSettings configuration = RunManagerEx.getInstanceEx(project).getSelectedConfiguration();
        if (configuration == null) {
            return false;
        }
        return configuration.getUniqueID().equals(this.activeConfigurationId);
    }

    /**
     * Try to start a new analysis job.
     *
     * @param project     the project for the current action
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
        RunConfiguration runConfiguration = configuration.getConfiguration();
        PythonRunConfiguration pythonRunConfiguration;
        if (runConfiguration instanceof PythonRunConfiguration) {
            pythonRunConfiguration = (PythonRunConfiguration) runConfiguration;
        } else {
            pythonRunConfiguration = null;
        }
        String inputFilePath =
                (pythonRunConfiguration == null || !pythonRunConfiguration.isRedirectInput())
                        ? null
                        : pythonRunConfiguration.getInputFile();
        DefaultRunExecutor executor = new DefaultRunExecutor();
        ExecutionEnvironmentBuilder builder = ExecutionEnvironmentBuilder.createOrNull(executor, configuration);
        if (builder == null) {
            return false;
        }
        ExecutionEnvironment environment = builder.activeTarget().dataContext(dataContext).build();
        try {
            RunProfileState state = environment.getState();
            if (!(state instanceof PythonCommandLineState)) {
                return false;
            }
            commandLineState = (PythonCommandLineState) state;
        } catch (com.intellij.execution.ExecutionException e1) {
            return false;
        }
        File plugins = new File(PathManager.getPluginsPath());
        File livePyPath = new File(plugins, "livepy");
        FilenameFilter jarFilter = (file, s) ->
                s.startsWith("livepy-") && s.endsWith(".jar");
        File jarParentPath;
        if (livePyPath.isDirectory()) {
            jarParentPath = new File(livePyPath, "lib");
        } else {
            jarParentPath = plugins;
        }
        File[] pluginFiles = jarParentPath.listFiles(jarFilter);
        if (pluginFiles == null || pluginFiles.length == 0) {
            throw new RuntimeException("Unable to find livepy-*.jar.");
        }
        File pythonPath = pluginFiles[0];
        commandLinePatcher = commandLine -> {
            Map<String, String> environment1 = commandLine.getEnvironment();
            commandLine.withInput(null);  // We send source code over stdin.

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
                oldPythonPath = "";
                newPythonPath = pythonPath.getAbsolutePath();
            } else {
                newPythonPath = pythonPath.getAbsolutePath() +
                        File.pathSeparator + oldPythonPath;
            }
            environment1.put("PYTHONPATH", newPythonPath);
            String modulePath = mainFile.getCanonicalPath();
            if (modulePath == null) {
                modulePath = mainFile.getPath();
            }
            boolean hasDriver = !driverPath.equals(modulePath);
            int i = 0;
            Rectangle bounds = canvasPainter.getBounds();
            boolean hasGoal = goalFile != null;
            int canvasHeight = hasGoal ? bounds.height / 2 : bounds.height;
            paramsGroup.addParameterAt(i++, "-m");
            paramsGroup.addParameterAt(i++, "space_tracer");
            String moduleName = hasDriver
                    ? getModuleName(new File(mainFile.getPath()), oldPythonPath)
                    : "__live_coding__";
            if (inputFilePath != null && inputFilePath.length() > 0) {
                paramsGroup.addParameterAt(i++, "--stdin");
                paramsGroup.addParameterAt(i++, inputFilePath);
            }
            String badDriverMessage = buildBadDriverMessage(configuration, moduleName);
            paramsGroup.addParameterAt(i++, "--bad_driver");
            paramsGroup.addParameterAt(i++, badDriverMessage);
            paramsGroup.addParameterAt(i++, "--canvas");
            paramsGroup.addParameterAt(i++, "--width");
            paramsGroup.addParameterAt(i++, Integer.toString(bounds.width));
            paramsGroup.addParameterAt(i++, "--height");
            paramsGroup.addParameterAt(i++, Integer.toString(canvasHeight));
            if (hasGoal) {
                paramsGroup.addParameterAt(i++, "--zoomed");
            }
            paramsGroup.addParameterAt(i++, "--live");
            paramsGroup.addParameterAt(i++, "--traced_file");
            paramsGroup.addParameterAt(i++, modulePath);
            paramsGroup.addParameterAt(i++, "--source_width");
            paramsGroup.addParameterAt(i++, "0");
            paramsGroup.addParameterAt(i++, "--traced");
            paramsGroup.addParameterAt(i++, moduleName);
            paramsGroup.addParameterAt(i, "--");
        };
        this.activeConfigurationId = configuration.getUniqueID();
        isRunning = true;
        schedule();
        return true;
    }

    void schedule() {
        if (!isRunning) {
            return;
        }
        Document document = getMainDocument();
        if (document != null) {
            schedule(document);
        }
    }

    @Nullable
    private Document getMainDocument() {
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        return documentManager.getDocument(mainFile);
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
    public void documentChanged(@NotNull DocumentEvent e) {
        if (isRunning) {
            FileDocumentManager documentManager =
                    FileDocumentManager.getInstance();
            Document[] unsavedDocuments = documentManager.getUnsavedDocuments();
            Document eventDocument = e.getDocument();
            for (Document unsavedDocument : unsavedDocuments) {
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
            checkGoalFile(sourceCode);
            String display = getDisplay(sourceCode);
            if (progressIndicator.isCanceled()) {
                return;
            }
            final String finalDisplay = display;
            ApplicationManager.getApplication().invokeLater(() -> displayResult(finalDisplay));
        } finally {
            progressIndicator.stop();
        }
    }

    private void checkGoalFile(String sourceCode) {
        Matcher matcher = GOAL_PATTERN.matcher(sourceCode);
        String newGoalFile = matcher.find()
                ? matcher.group(1)
                : null;
        Rectangle newBounds = canvasPainter.getBounds();
        if (Objects.equals(newGoalFile, goalFile) && newBounds.equals(goalBounds)) {
            return;
        }
        goalBounds = newBounds;
        goalFile = newGoalFile;
        if (newGoalFile == null) {
            goalImageCommand = null;
            return;
        }
        VirtualFile goalPath = mainFile.getParent().findFileByRelativePath(newGoalFile);
        if (goalPath == null) {
            log.error("Unable to load goal file " + newGoalFile);
            goalImageCommand = null;
            return;
        }
        StringWriter goalSource = new StringWriter();
        PrintWriter goalSourcePrinter = new PrintWriter(goalSource);
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                    goalPath.getInputStream()));
            String line;
            while (null != (line = reader.readLine())) {
                goalSourcePrinter.println(line);
            }
        } catch (IOException e) {
            log.error(e);
            return;
        }
        String goalDisplay = getDisplay(goalSource.toString());
        if (goalDisplay.startsWith(CANVAS_START)) {
            BufferedReader reader =
                    new BufferedReader(new StringReader(goalDisplay));
            try {
                reader.readLine();  // Skip first line.
                CanvasReader canvasReader = new CanvasReader(reader);
                ArrayList<CanvasCommand> canvasCommands = canvasReader.readCommands();
                for (CanvasCommand command : canvasCommands) {
                    if (command.getName().equals(CanvasCommand.CREATE_IMAGE)) {
                        goalImageCommand = command;
                        return;
                    }
                }
            } catch (IOException e) {
                log.error(e);
                return;
            }
        }
        goalImageCommand = null;
    }

    @NotNull
    private String getDisplay(String sourceCode) {
        String display = "";
        try {
            CapturingProcessHandler processHandler = startProcess(sourceCode);
            ProcessOutput processOutput =
                    processHandler.runProcessWithProgressIndicator(progressIndicator);
            display = processOutput.getStdout();
            String stderr = processOutput.getStderr();
            isPassing = processOutput.getExitCode() == 0;
            if (stderr.length() > 0) {
                display += "\nLive coding plugin error:\n" + stderr;
            }
        } catch (ExecutionException | IOException ex) {
            display += "\nLive coding plugin exception:\n" + ex;
            log.error("Report failed.", ex);
        }
        return display;
    }

    private void schedule(Document document) {
        if (progressIndicator.isRunning()) {
            progressIndicator.cancel();
        }
        alarm.cancelAllRequests();
        alarm.addRequest(() -> pool.submit(() -> analyse(document)), 300);
    }

    private void displayResult(String display) {
        StringWriter writer = new StringWriter();
        BufferedReader reader = new BufferedReader(new StringReader(display));
        try {
            String line = reader.readLine();  // Skip first line.
            if (line == null) {
                line = "";
            }
            if (!CANVAS_START.equals(line)) {
                canvasPainter.setCommands(null);
                writer.write(line);
                writer.write('\n');
            } else {
                CanvasReader canvasReader = new CanvasReader(reader);
                ArrayList<CanvasCommand> canvasCommands = canvasReader.readCommands();
                if (goalImageCommand != null) {
                    boolean isSolved = false;
                    for (CanvasCommand command : canvasCommands) {
                        if (command.getName().equals(CanvasCommand.CREATE_IMAGE) &&
                                command.getOption("image").equals(
                                        goalImageCommand.getOption("image"))) {
                            isSolved = true;
                            break;
                        }
                    }
                    Rectangle canvasBounds = canvasPainter.getBounds();
                    if (isSolved) {
                        CanvasCommand message = new CanvasCommand();
                        message.setName(CanvasCommand.CREATE_TEXT);
                        message.setOption("text", "Solved!");
                        message.setOption("font", "('Arial', 16, 'normal')");
                        message.addCoordinate(canvasBounds.width / 2);
                        message.addCoordinate(canvasBounds.height * 3 / 4);
                        canvasCommands.add(message);
                    } else {
                        goalImageCommand.setCoordinate(1, canvasBounds.height / 2);
                        canvasCommands.add(goalImageCommand);
                    }
                }
                canvasPainter.setCommands(canvasCommands);
            }
            while (true) {
                line = reader.readLine();
                if (line == null) {
                    break;
                }
                writer.write(line);
                writer.write('\n');
            }
            display = writer.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        final String finalDisplay = display;
        isDisplayUpdating = true;

        ApplicationManager.getApplication().runWriteAction(
                () -> displayDocument.setText(finalDisplay));
        synchronizeInlays();

        var editor = getEditor();
        Project project = editor.getProject();
        if (project != null) {
            DaemonCodeAnalyzer.getInstance(project).restart();
        }
        if (editor.getSettings().isUseSoftWraps()) {
            AbstractToggleUseSoftWrapsAction.toggleSoftWraps(
                    editor,
                    null,
                    false);
        }

        alarm.addRequest(() -> isDisplayUpdating = false, 300);
    }

    private void synchronizeInlays() {
        final var inlayLines = new HashSet<Integer>();
        var mainDocument = getMainDocument();
        if (mainDocument != null) {
            EditorImpl mainEditor = getEditor(mainDocument);
            @NotNull List<Inlay<?>> mainInlays =
                    mainEditor.getInlayModel().getBlockElementsInRange(0, Integer.MAX_VALUE);
            for (var inlay: mainInlays) {
                int mainLine = mainDocument.getLineNumber(inlay.getOffset());
                inlayLines.add(mainLine);
            }
        }
        var displayEditor = getEditor();
        var displayInlayModel = displayEditor.getInlayModel();
        var displayInlays = displayInlayModel.getBlockElementsInRange(
                0,
                Integer.MAX_VALUE);
        for (var inlay : displayInlays) {
            int displayLine = displayDocument.getLineNumber(inlay.getOffset());
            if ( ! inlayLines.remove(displayLine)) {
                // line no longer has an inlay, so remove the existing one.
                boolean above = true;
                var lineInlays = displayInlayModel.getBlockElementsForVisualLine(
                        displayLine,
                        above);
                for (var toRemove : lineInlays) {
                    toRemove.dispose();
                }
            }
        }
        for (var lineNum: inlayLines) {
            int lineOffset;
            try {
                lineOffset = displayDocument.getLineStartOffset(lineNum);
            } catch (IndexOutOfBoundsException ex) {
                continue;
            }
            var relatesToPreceding = true;
            var showAbove = true;
            var priority = 1;
            displayInlayModel.addBlockElement(
                    lineOffset,
                    relatesToPreceding,
                    showAbove,
                    priority,
                    inlay -> 0);
        }
    }

    EditorImpl getEditor() {
        return getEditor(getDisplayDocument());
    }

    private static EditorImpl getEditor(Document document) {
        Editor[] editors = EditorFactory.getInstance().getEditors(document);
        if (editors.length >= 1) {
            return (EditorImpl) editors[0];
        }
        throw new RuntimeException("No editor available.");
    }

    private CapturingProcessHandler startProcess(String sourceCode) throws ExecutionException, IOException {
        final GeneralCommandLine commandLine =
                ApplicationManager.getApplication().runReadAction(
                        (Computable<GeneralCommandLine>) () ->
                                commandLineState.generateCommandLine(
                                        new CommandLinePatcher[]{commandLinePatcher}));

        /* For some reason, the original command line doesn't work when this
         * plugin gets installed in IDEs other than PyCharm. Copying all the
         * parameters and other settings to a new command line works, though.
         */
        final GeneralCommandLine commandLine2 = new GeneralCommandLine();
        commandLine2.addParameters(commandLine.getParametersList().getList());
        commandLine2.setExePath(commandLine.getExePath());
        commandLine2.setCharset(commandLine.getCharset());
        commandLine2.setWorkDirectory(commandLine.getWorkDirectory());
        commandLine2.getEnvironment().putAll(commandLine.getEnvironment());

        final CapturingProcessHandler processHandler = new CapturingProcessHandler(commandLine2);
        try {
            byte[] stdin = sourceCode.getBytes(StandardCharsets.UTF_8);
            final OutputStream processInput = processHandler.getProcessInput();
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
        Path absolutePath = file.toPath();
        Path packagePath = absolutePath.getParent();
        Path initPath = packagePath.resolve("__init__.py");
        if (!initPath.toFile().exists()) {
            // Traced file isn't in a package, so just use the file name.
            String fileName = absolutePath.getFileName().toString();
            if (fileName.endsWith(".py")) {
                fileName = fileName.substring(0, fileName.length() - 3);
            }
            return fileName;
        }
        String[] paths = pythonPath.split(File.pathSeparator);
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
