package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnActionEvent;
import icons.LivePythonIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class TurtleAction extends LayoutAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        SplitFileEditor splitFileEditor = getEditor(e);
        if (splitFileEditor == null) {
            return;
        }
        splitFileEditor.startTurtle(e.getProject(), e.getDataContext());
    }

    @Override
    SplitFileEditor.SplitEditorLayout getActiveLayout() {
        return SplitFileEditor.SplitEditorLayout.TURTLE;
    }

    @Override
    Icon getDefaultIcon() {
        return LivePythonIcons.TURTLE;
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
            SplitFileEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        if (currentLayout != SplitFileEditor.SplitEditorLayout.DISPLAY) {
            return true;
        }
        return isRunningSelectedConfiguration;
    }
}
