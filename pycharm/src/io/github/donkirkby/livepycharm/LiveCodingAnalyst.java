package io.github.donkirkby.livepycharm;

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

    LiveCodingAnalyst(VirtualFile mainFile, Document displayDocument) {
        this.mainFile = mainFile;
        this.displayDocument = displayDocument;
    }

    public void start() {
        Document mainDocument = getDocument();
        if (mainDocument != null && displayDocument != null) {
            mainDocument.addDocumentListener(this);
        }
    }

    public void stop() {
        Document mainDocument = getDocument();
        if (mainDocument != null && displayDocument != null) {
            mainDocument.removeDocumentListener(this);
        }
    }

    private Document getDocument() {
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        return documentManager.getDocument(mainFile);
    }

    @Override
    public void documentChanged(DocumentEvent e) {
        StringBuilder builder = new StringBuilder();
        File plugins = new File(PathManager.getPluginsPath());
        File livePyPath = new File(plugins, "livepy");
        File pySrc = new File(livePyPath, "classes");
        File codeTracer = new File(pySrc, "code_tracer.py");
        File workingDir = new File(
                mainFile.getParent().getPath());
        ProcessBuilder processBuilder = new ProcessBuilder(
                "python",
                codeTracer.getAbsolutePath())
                .directory(workingDir)
                .redirectInput(ProcessBuilder.Redirect.PIPE)
                .redirectOutput(ProcessBuilder.Redirect.PIPE)
                .redirectErrorStream(true);
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
