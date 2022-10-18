package io.github.donkirkby.livepycharm;

import com.intellij.codeInsight.hints.FactoryInlayHintsCollector;
import com.intellij.codeInsight.hints.InlayHintsSink;
import com.intellij.codeInsight.hints.InlayPresentationFactory;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPlainText;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings({"UnstableApiUsage", "MissingRecentApi"})
public class DisplayInlayHintsCollector extends FactoryInlayHintsCollector {
    public DisplayInlayHintsCollector(@NotNull Editor editor) {
        super(editor);
    }

    @Override
    public boolean collect(
            @NotNull PsiElement psiElement,
            @NotNull Editor editor,
            @NotNull InlayHintsSink sink) {

        if ( ! (psiElement instanceof PsiPlainText)) {
            return true;
        }
        var document = editor.getDocument();
        var inlayLines =
                document.getUserData(LiveCodingAnalyst.INLAY_LINES_KEY);
        if (inlayLines == null) {
            return false;
        }
        for (var lineNum: inlayLines) {
            var lineOffset = document.getLineStartOffset(lineNum);
            var relatesToPreceding = true;
            var showAbove = true;
            var priority = 1;
            sink.addBlockElement(
                    lineOffset,
                    relatesToPreceding,
                    showAbove,
                    priority,
                    getFactory().container(
                            getFactory().textSpacePlaceholder(1, false),
                            new InlayPresentationFactory.Padding(0, 0, 5, 4),
                            null,
                            null,
                            1));
        }
        return false;
    }
}
