package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.editor.Editor;

public class TurtleAction extends AnAction {

    @Override
    public void actionPerformed(AnActionEvent e) {
        Editor editor = e.getData(LangDataKeys.EDITOR);

        SplitFileEditor splitFileEditor = SplitFileEditor.getSplitFileEditor(editor);
        if (splitFileEditor == null) {
            return;
        }
        splitFileEditor.startTurtle(e.getProject(), e.getDataContext());
    }
}
