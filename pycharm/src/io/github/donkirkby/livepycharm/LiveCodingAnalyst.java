package io.github.donkirkby.livepycharm;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.event.DocumentAdapter;
import com.intellij.openapi.editor.event.DocumentEvent;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

public class LiveCodingAnalyst extends DocumentAdapter {
    private static Logger log = Logger.getInstance(LiveCodingAnalyst.class);
    private Document displayDocument;

    LiveCodingAnalyst(Document displayDocument) {
        this.displayDocument = displayDocument;
    }

    @Override
    public void documentChanged(DocumentEvent e) {
        ApplicationManager.getApplication().runWriteAction(
                () -> {
                    StringBuilder builder = new StringBuilder();
                    BufferedReader reader = new BufferedReader(
                            new StringReader(e.getDocument().getText()));
                    String line;
                    try {
                        while (null != (line = reader.readLine())) {
                            line = line.replaceAll("^\\s*", "");
                            if (line.length() > 0) {
                                builder.append(line.substring(0, 1));
                                int n = line.length();
                                builder.append(line.substring(n - 1, n));
                            }
                            builder.append("\n");
                        }
                    } catch (IOException ex) {
                        log.error("Report failed.", ex);
                    }
                    displayDocument.setText(builder.toString());
                });
    }
}
