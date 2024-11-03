package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.*;
import icons.LivePythonIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class StartAction extends LayoutAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        LivePythonEditor livePythonEditor = getEditor(e);
        if (livePythonEditor == null) {
            return;
        }
        livePythonEditor.startAnalysis(e.getProject(), e.getDataContext());
    }

    @Override
    LivePythonEditor.SplitEditorLayout getActiveLayout() {
        return LivePythonEditor.SplitEditorLayout.DISPLAY;
    }

    @Override
    Icon getDefaultIcon() {
        return LivePythonIcons.MEDIA_PLAY;
    }

    @Override
    Icon getDisabledIcon() {
        return LivePythonIcons.MEDIA_PLAY_DISABLED;
    }

    @Override
    Icon getPassingIcon() {
        return LivePythonIcons.MEDIA_PLAY_GREEN;
    }

    @Override
    Icon getFailingIcon() {
        return LivePythonIcons.MEDIA_PLAY_RED;
    }

    @Override
    boolean isEnabled(
            LivePythonEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        if (currentLayout != LivePythonEditor.SplitEditorLayout.TURTLE) {
            return true;
        }
        return isRunningSelectedConfiguration;
    }
}
