package io.github.donkirkby.livepycharm;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.event.DocumentAdapter;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.vfs.VirtualFile;

import java.io.*;

public class LiveCodingAnalyst extends DocumentAdapter {
    private static Logger log = Logger.getInstance(LiveCodingAnalyst.class);
    private final VirtualFile mainFile;
    private final Document displayDocument;
    private boolean isRunning;

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

    @Override
    public void documentChanged(DocumentEvent e) {
        if ( ! isRunning) {
            return;
        }
        StringBuilder builder = new StringBuilder();
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
            outputStream.write(e.getDocument().getText().getBytes());
            outputStream.close();
            InputStream inputStream = process.getInputStream();
            BufferedReader reader =
                    new BufferedReader(new InputStreamReader(inputStream));
            String line;
            while (null != (line = reader.readLine())) {
                builder.append(line);
                builder.append("\n");
            }
        } catch (IOException ex) {
            log.error("Report failed.", ex);
        }

        ApplicationManager.getApplication().runWriteAction(
                () -> displayDocument.setText(builder.toString()));
    }
}
