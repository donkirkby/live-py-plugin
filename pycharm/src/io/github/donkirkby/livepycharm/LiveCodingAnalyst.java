package io.github.donkirkby.livepycharm;

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
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.concurrent.*;

public class LiveCodingAnalyst extends DocumentAdapter {
    private static Logger log = Logger.getInstance(LiveCodingAnalyst.class);
    private static ExecutorService pool = Executors.newCachedThreadPool();
    private final VirtualFile mainFile;
    private final Document displayDocument;
    private boolean isRunning;
    private String prevSourceCode;  // last source code analysed without being cancelled

    LiveCodingAnalyst(VirtualFile mainFile, Document displayDocument, Disposable parent) {
        this.mainFile = mainFile;
        this.displayDocument = displayDocument;
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        Document document = documentManager.getDocument(mainFile);
        if (document != null) {
            document.addDocumentListener(this, parent);
        }
    }

    void start() {
        isRunning = true;
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
            ProgressIndicatorUtils.scheduleWithWriteActionPriority(new AnalysisTask(document));
        }
    }

    @Override
    public void documentChanged(DocumentEvent e) {
        if ( ! isRunning) {
            return;
        }
        ProgressIndicatorUtils.scheduleWithWriteActionPriority(new AnalysisTask(e.getDocument()));
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
        File workingDir = new File(
                mainFile.getParent().getPath());
        ProcessBuilder processBuilder = new ProcessBuilder(
                "python",
                "-m",
                "code_tracer")
                .directory(workingDir)
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
