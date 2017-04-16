package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.editor.Editor;

public class StartAction extends AnAction {

    @Override
    public void actionPerformed(AnActionEvent e) {
        Editor editor = e.getData(LangDataKeys.EDITOR);

        SplitFileEditor splitFileEditor = SplitFileEditor.getSplitFileEditor(editor);
        if (splitFileEditor != null) {
            splitFileEditor.startAnalysis();
        }
    }
}
