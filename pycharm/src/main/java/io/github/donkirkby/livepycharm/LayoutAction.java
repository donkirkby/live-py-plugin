package io.github.donkirkby.livepycharm;

import com.intellij.ide.lightEdit.LightEditCompatible;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.DumbAware;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

abstract class LayoutAction extends AnAction implements DumbAware, Toggleable, LightEditCompatible {
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

    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
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
