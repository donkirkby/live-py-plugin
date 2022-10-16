package io.github.donkirkby.livepycharm;

import com.intellij.codeInsight.hints.FactoryInlayHintsCollector;
import com.intellij.codeInsight.hints.InlayHintsSink;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPlainText;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("UnstableApiUsage")
public class DisplayInlayHintsCollector extends FactoryInlayHintsCollector {
    public DisplayInlayHintsCollector(@NotNull Editor editor) {
        super(editor);
    }

    @Override
    public boolean collect(
            @NotNull PsiElement psiElement,
            @NotNull Editor editor,
            @NotNull InlayHintsSink sink) {

        if (psiElement instanceof PsiPlainText) {
            var textElement = (PsiPlainText) psiElement;
            var text = textElement.getText();
            if (text.length() == 0) {
                return true;
            }
            var document = editor.getDocument();
            var lineOffset = document.getLineStartOffset(20);
            var relatesToPreceding = false;
            var showAbove = true;
            var priority = 0;
            sink.addBlockElement(
                    lineOffset,
                    relatesToPreceding,
                    showAbove,
                    priority,
                    getFactory().smallText("Foo"));
        }
        return true;
    }
}
