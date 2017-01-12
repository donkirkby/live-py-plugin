package io.github.donkirkby.livepycharm;

import com.intellij.codeInsight.editorActions.TypedHandlerDelegate;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.WeakHashMap;

public class TypedHandler extends TypedHandlerDelegate {
    // maps Python files to their display documents.
    private static Map<VirtualFile, Document> displayDocuments =
            new WeakHashMap<>();

    static void registerDocument(VirtualFile file, Document document) {
        displayDocuments.put(file, document);
    }

    @Override
    public Result charTyped(
            char c,
            Project project,
            @NotNull Editor editor,
            @NotNull PsiFile file) {
        Logger.getInstance(TypedHandler.class).info("typed " + c);
        Document document = displayDocuments.get(file.getVirtualFile());
        if (document != null) {
            ApplicationManager.getApplication().runWriteAction(
                    () -> document.insertString(
                            document.getTextLength(),
                            "typed " + c + "\n"));
        }
        return super.charTyped(c, project, editor, file);
    }
}
