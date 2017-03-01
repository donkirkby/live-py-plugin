package io.github.donkirkby.livepycharm;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.editor.ex.DocumentEx;
import com.intellij.openapi.editor.impl.EditorImpl;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.vfs.VirtualFile;

public class StopAction extends AnAction {

    @Override
    public void actionPerformed(AnActionEvent e) {
        Object x = e.getData(LangDataKeys.EDITOR);
        EditorImpl x2 = (EditorImpl)x;
        DocumentEx doc = x2.getDocument();
        VirtualFile file = FileDocumentManager.getInstance().getFile(doc);

        SplitFileEditor editor = (SplitFileEditor) FileEditorManager.getInstance(e.getProject()).getSelectedEditor(file);
        editor.triggerLayoutChange(SplitFileEditor.SplitEditorLayout.FIRST);
    }
}
