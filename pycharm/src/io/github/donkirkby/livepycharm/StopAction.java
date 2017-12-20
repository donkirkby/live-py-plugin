package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnActionEvent;
import icons.LivePythonIcons;

import javax.swing.*;

public class StopAction extends LayoutAction {

    @Override
    public void actionPerformed(AnActionEvent e) {
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
    boolean isEnabled(boolean isActive) {
        return ! isActive;
    }
}
