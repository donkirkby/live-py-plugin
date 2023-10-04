package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

abstract class LayoutAction extends AnAction {
    abstract SplitFileEditor.SplitEditorLayout getActiveLayout();

    abstract Icon getDefaultIcon();

    Icon getPassingIcon() {
        return getDefaultIcon();
    }

    Icon getFailingIcon() {
        return getDefaultIcon();
    }

    Icon getDisabledIcon() {
        return getDefaultIcon();
    }

    boolean isEnabled(
            SplitFileEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        return true;
    }

    SplitFileEditor getEditor(AnActionEvent e) {
        Editor editor = e.getData(LangDataKeys.EDITOR);

        return SplitFileEditor.getSplitFileEditor(editor);
    }

    @Override
    public void update(@NotNull AnActionEvent e) {
        SplitFileEditor splitFileEditor = getEditor(e);
        Icon icon = getDefaultIcon();
        SplitFileEditor.SplitEditorLayout currentLayout =
                splitFileEditor == null
                        ? null
                        : splitFileEditor.getLayout();
        boolean isLayoutActive =
                splitFileEditor != null &&
                        currentLayout == getActiveLayout();
        if (isLayoutActive) {
            icon = splitFileEditor.isAnalysisPassing()
                    ? getPassingIcon()
                    : getFailingIcon();
        }
        boolean isRunningSelectedConfiguration =
                splitFileEditor != null &&
                        splitFileEditor.isRunningSelectedConfiguration(e.getProject());
        boolean enabled = splitFileEditor != null && isEnabled(
                currentLayout,
                isRunningSelectedConfiguration);
        e.getPresentation().setEnabled(enabled);
        if ( ! enabled) {
            icon = getDisabledIcon();
        }
        e.getPresentation().setIcon(icon);
    }
}
