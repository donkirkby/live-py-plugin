package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnActionEvent;
import icons.LivePythonIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class StopAction extends LayoutAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        SplitFileEditor splitFileEditor = getEditor(e);
        if (splitFileEditor == null) {
            return;
        }
        splitFileEditor.stopAnalysis();
    }

    @Override
    SplitFileEditor.SplitEditorLayout getActiveLayout() {
        return SplitFileEditor.SplitEditorLayout.SINGLE;
    }

    @Override
    Icon getDefaultIcon() {
        return LivePythonIcons.MEDIA_STOP;
    }

    @Override
    Icon getDisabledIcon() {
        return LivePythonIcons.MEDIA_STOP_DISABLED;
    }

    @Override
    boolean isEnabled(
            SplitFileEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        return currentLayout != SplitFileEditor.SplitEditorLayout.SINGLE;
    }
}
