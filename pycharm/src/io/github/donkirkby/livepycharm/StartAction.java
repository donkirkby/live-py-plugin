package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.*;
import icons.LivePythonIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class StartAction extends LayoutAction {

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        SplitFileEditor splitFileEditor = getEditor(e);
        if (splitFileEditor == null) {
            return;
        }
        splitFileEditor.startAnalysis(e.getProject(), e.getDataContext());
    }

    @Override
    SplitFileEditor.SplitEditorLayout getActiveLayout() {
        return SplitFileEditor.SplitEditorLayout.DISPLAY;
    }

    @Override
    Icon getDefaultIcon() {
        return LivePythonIcons.MEDIA_PLAY;
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
            SplitFileEditor.SplitEditorLayout currentLayout,
            boolean isRunningSelectedConfiguration) {
        if (currentLayout != SplitFileEditor.SplitEditorLayout.TURTLE) {
            return true;
        }
        return isRunningSelectedConfiguration;
    }
}
