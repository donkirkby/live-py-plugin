package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.editor.Editor;

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

    boolean isEnabled(boolean isActive) {
        return true;
    }

    SplitFileEditor getEditor(AnActionEvent e) {
        Editor editor = e.getData(LangDataKeys.EDITOR);

        return SplitFileEditor.getSplitFileEditor(editor);
    }

    @Override
    public void update(AnActionEvent e) {
        SplitFileEditor splitFileEditor = getEditor(e);
        Icon icon = getDefaultIcon();
        boolean isActive = splitFileEditor != null &&
                splitFileEditor.getLayout() == getActiveLayout();
        if (isActive) {
            icon = splitFileEditor.isAnalysisPassing()
                    ? getPassingIcon()
                    : getFailingIcon();
        }
        e.getPresentation().setIcon(icon);
        e.getPresentation().setEnabled(isEnabled(isActive));
    }
}
