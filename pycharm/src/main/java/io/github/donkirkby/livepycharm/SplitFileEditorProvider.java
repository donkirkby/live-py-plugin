package io.github.donkirkby.livepycharm;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.*;
import com.intellij.openapi.fileEditor.*;
import com.intellij.openapi.fileEditor.impl.text.PsiAwareTextEditorProvider;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightVirtualFile;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

public class SplitFileEditorProvider implements FileEditorProvider, DumbAware {
    private static final String FIRST_EDITOR = "first_editor";
    private static final String SECOND_EDITOR = "second_editor";
    private static final String SPLIT_LAYOUT = "split_layout";

    @NotNull
    private final com.intellij.openapi.fileEditor.FileEditorProvider myFirstProvider;
    @NotNull
    private final com.intellij.openapi.fileEditor.FileEditorProvider mySecondProvider;

    @NotNull
    private final String myEditorTypeId;

    public SplitFileEditorProvider() {
        myFirstProvider = new PsiAwareTextEditorProvider();
        mySecondProvider = new PsiAwareTextEditorProvider();

        myEditorTypeId = "live-pycharm";
    }

    @Override
    public boolean accept(@NotNull Project project, @NotNull VirtualFile file) {
        return myFirstProvider.accept(project, file) &&
                mySecondProvider.accept(project, file) &&
                "py".equals(file.getExtension());
    }

    @NotNull
    @Override
    public FileEditor createEditor(@NotNull Project project, @NotNull VirtualFile file) {
        LightVirtualFile displayFile = new LightVirtualFile(
                file.getName() + "-display.txt",
                FileTypes.PLAIN_TEXT,
                "created for " + file.getName() + "\n");
        FileEditor first = myFirstProvider.createEditor(project, file);
        FileEditor second = mySecondProvider.createEditor(project, displayFile);
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
        Document displayDocument = ApplicationManager.getApplication().runReadAction(
                (Computable<Document>) () -> documentManager.getDocument(displayFile));
        return createSplitEditor(first, second, file, displayDocument);
    }

    @NotNull
                            @Override
    public String getEditorTypeId() {
        return myEditorTypeId;
    }


    @NotNull
    @Override
    public FileEditorState readState(@NotNull Element sourceElement, @NotNull Project project, @NotNull VirtualFile file) {
        Element child = sourceElement.getChild(FIRST_EDITOR);
        FileEditorState firstState = null;
        if (child != null) {
            firstState = myFirstProvider.readState(child, project, file);
        }
        child = sourceElement.getChild(SECOND_EDITOR);
        FileEditorState secondState = null;
        if (child != null) {
            secondState = mySecondProvider.readState(child, project, file);
        }

        final Attribute attribute = sourceElement.getAttribute(SPLIT_LAYOUT);

        final String layoutName;
        if (attribute != null) {
            layoutName = attribute.getValue();
        } else {
            layoutName = null;
        }

        return new SplitFileEditor.MyFileEditorState(layoutName, firstState, secondState);
    }

    @Override
    public void writeState(@NotNull FileEditorState state, @NotNull Project project, @NotNull Element targetElement) {
        if (!(state instanceof SplitFileEditor.MyFileEditorState)) {
            return;
        }
        final SplitFileEditor.MyFileEditorState compositeState = (SplitFileEditor.MyFileEditorState) state;

        Element child = new Element(FIRST_EDITOR);
        if (compositeState.getFirstState() != null) {
            myFirstProvider.writeState(compositeState.getFirstState(), project, child);
            targetElement.addContent(child);
        }

        child = new Element(SECOND_EDITOR);
        if (compositeState.getSecondState() != null) {
            mySecondProvider.writeState(compositeState.getSecondState(), project, child);
            targetElement.addContent(child);
        }

        if (compositeState.getSplitLayout() != null) {
            targetElement.setAttribute(SPLIT_LAYOUT, compositeState.getSplitLayout());
        }
    }

    private FileEditor createSplitEditor(
            @NotNull final FileEditor firstEditor,
            @NotNull FileEditor secondEditor,
            VirtualFile file,
            Document displayDocument) {
        return new SplitFileEditor(firstEditor, secondEditor, file, displayDocument);
    }

    @NotNull
    @Override
    public FileEditorPolicy getPolicy() {
        return FileEditorPolicy.HIDE_DEFAULT_EDITOR;
    }

    private static boolean runsInProjectView()
    {
        return !(ApplicationManager.getApplication().isDispatchThread());
    }

    private static boolean runsInEventDispatchThread()
    {
        return ApplicationManager.getApplication().isWriteAccessAllowed();
    }
}

