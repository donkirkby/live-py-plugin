package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

abstract class LayoutAction extends AnAction {
    abstract LivePythonEditor.SplitEditorLayout getActiveLayout();

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

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }

    boolean isEnabled(
            LivePythonEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        return true;
    }

    LivePythonEditor getEditor(AnActionEvent e) {
        Editor editor = e.getData(LangDataKeys.EDITOR);

        return LivePythonEditor.getSplitFileEditor(editor);
    }

    @Override
    public void update(@NotNull AnActionEvent e) {
        LivePythonEditor livePythonEditor = getEditor(e);
        Icon icon = getDefaultIcon();
        LivePythonEditor.SplitEditorLayout currentLayout =
                livePythonEditor == null
                        ? null
                        : livePythonEditor.getLayout();
        boolean isLayoutActive =
                livePythonEditor != null &&
                        currentLayout == getActiveLayout();
        if (isLayoutActive) {
            icon = livePythonEditor.isAnalysisPassing()
                    ? getPassingIcon()
                    : getFailingIcon();
        }
        boolean isRunningSelectedConfiguration =
                livePythonEditor != null &&
                        livePythonEditor.isRunningSelectedConfiguration(e.getProject());
        boolean enabled = livePythonEditor != null && isEnabled(
                currentLayout,
                isRunningSelectedConfiguration);
        e.getPresentation().setEnabled(enabled);
        if ( ! enabled) {
            icon = getDisabledIcon();
        }
        e.getPresentation().setIcon(icon);
    }
}
