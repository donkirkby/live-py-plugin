package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnActionEvent;
import icons.LivePythonIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class StopAction extends LayoutAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        LivePythonEditor livePythonEditor = getEditor(e);
        if (livePythonEditor == null) {
            return;
        }
        livePythonEditor.stopAnalysis();
    }

    @Override
    LivePythonEditor.SplitEditorLayout getActiveLayout() {
        return LivePythonEditor.SplitEditorLayout.SINGLE;
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
            LivePythonEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        return currentLayout != LivePythonEditor.SplitEditorLayout.SINGLE;
    }
}
