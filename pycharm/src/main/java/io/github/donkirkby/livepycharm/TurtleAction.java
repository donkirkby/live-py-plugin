package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnActionEvent;
import icons.LivePythonIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class TurtleAction extends LayoutAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        LivePythonEditor livePythonEditor = getEditor(e);
        if (livePythonEditor == null) {
            return;
        }
        livePythonEditor.startTurtle(e.getProject(), e.getDataContext());
    }

    @Override
    LivePythonEditor.SplitEditorLayout getActiveLayout() {
        return LivePythonEditor.SplitEditorLayout.TURTLE;
    }

    @Override
    Icon getDefaultIcon() {
        return LivePythonIcons.TURTLE;
    }

    @Override
    Icon getDisabledIcon() {
        return LivePythonIcons.TURTLE_DISABLED;
    }

    @Override
    Icon getPassingIcon() {
        return LivePythonIcons.TURTLE_GREEN;
    }

    @Override
    Icon getFailingIcon() {
        return LivePythonIcons.TURTLE_RED;
    }

    @Override
    boolean isEnabled(
            LivePythonEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        if (currentLayout != LivePythonEditor.SplitEditorLayout.DISPLAY) {
            return true;
        }
        return isRunningSelectedConfiguration;
    }
}
