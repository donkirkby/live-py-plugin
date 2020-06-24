package io.github.donkirkby.livepycharm;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.editor.*;
import com.intellij.openapi.editor.event.*;
import com.intellij.openapi.fileEditor.*;
import com.intellij.openapi.fileEditor.impl.text.PsiAwareTextEditorProvider;
import com.intellij.openapi.fileEditor.impl.text.TextEditorProvider;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.util.Alarm;
import org.jdom.Attribute;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

public class SplitFileEditorProvider extends TextEditorProvider {
    private static final String FIRST_EDITOR = "first_editor";
    private static final String SECOND_EDITOR = "second_editor";
    private static final String SPLIT_LAYOUT = "split_layout";

    @NotNull
    private final com.intellij.openapi.fileEditor.FileEditorProvider myFirstProvider;
    @NotNull
    private final com.intellij.openapi.fileEditor.FileEditorProvider mySecondProvider;

    @NotNull
    private final String myEditorTypeId;

    private class Builder {
        private Editor mainEditor;
        private Editor displayEditor;
        private Editor scrollingEditor;
        private int displayX;
        private Alarm alarm = new Alarm();
        private Project project;
        private VirtualFile file;

        Builder(Project project, VirtualFile file) {
            this.project = project;
            this.file = file;
        }

        public FileEditor build() {
            LightVirtualFile displayFile = new LightVirtualFile(
                    file.getName(),
                    FileTypes.PLAIN_TEXT,
                    "created for " + file.getName() + "\n");
            FileDocumentManager documentManager = FileDocumentManager.getInstance();
            Document mainDocument = documentManager.getDocument(file);
            Document displayDocument = documentManager.getDocument(displayFile);
            Disposable disposable = () -> {};
            EditorFactory.getInstance().addEditorFactoryListener(
                    new EditorFactoryListener() {
                        @Override
                        public void editorCreated(@NotNull EditorFactoryEvent event) {
                            Editor editor = event.getEditor();
                            Document document = editor.getDocument();
                            if (mainEditor == null &&
                                    document == mainDocument) {
                                mainEditor = editor;
                            } else if (displayEditor == null &&
                                    document == displayDocument) {
                                displayEditor = editor;
                            }
                        }
                    },
                    disposable
            );
            FileEditor editor = createSplitEditor(
                    myFirstProvider.createEditor(project, file),
                    mySecondProvider.createEditor(project, displayFile),
                    file,
                    displayDocument);
            Editor mainEditor = this.mainEditor;
            Editor displayEditor = this.displayEditor;
            if (mainEditor != null && displayEditor != null) {
                mainEditor.getScrollingModel().addVisibleAreaListener(
                        e -> updateScrolling(mainEditor));
                displayEditor.getScrollingModel().addVisibleAreaListener(
                        e -> updateScrolling(displayEditor));

                if (displayDocument != null) {
                    displayDocument.addDocumentListener(new DocumentListener() {
                        @Override
                        public void documentChanged(@NotNull DocumentEvent event) {
                            updateDisplayFolding(mainEditor, displayEditor);
                        }
                    });
                }
            }
            return editor;
        }

        private FileEditor createSplitEditor(
                @NotNull final FileEditor firstEditor,
                @NotNull FileEditor secondEditor,
                VirtualFile file,
                Document displayDocument) {
            return new SplitFileEditor(firstEditor, secondEditor, file, displayDocument);
        }

        private void updateScrolling(Editor activeEditor) {
            // Horizontal scroll remembers manually scrolled position.
            SplitFileEditor splitFileEditor =
                    SplitFileEditor.getSplitFileEditor(mainEditor);
            ScrollingModel displayScroll =
                    displayEditor.getScrollingModel();

            boolean isUpdating = splitFileEditor != null &&
                    splitFileEditor.isDisplayUpdating();
            boolean isDisplayHidden = splitFileEditor != null &&
                    splitFileEditor.getLayout() !=
                            SplitFileEditor.SplitEditorLayout.DISPLAY;
            if (isUpdating) {
                displayScroll.disableAnimation();
                displayScroll.scrollHorizontally(displayX);
                displayScroll.enableAnimation();
            } else {
                displayX = displayScroll.getVisibleArea().x;
            }

            // Vertical scroll synchronized between two sides.
            Editor followingEditor;
            if (scrollingEditor != null) {
                activeEditor = scrollingEditor;
                followingEditor = activeEditor == mainEditor
                        ? displayEditor
                        : mainEditor;
            } else if (activeEditor == mainEditor || isUpdating || isDisplayHidden) {
                activeEditor = mainEditor;
                followingEditor = displayEditor;
            } else {
                followingEditor = mainEditor;
            }
            recordScrollingEditor(activeEditor);

            ScrollingModel leadingScroll = activeEditor.getScrollingModel();
            ScrollingModel followingScroll = followingEditor.getScrollingModel();
            int scrollOffset = leadingScroll.getVisibleArea().y;
            followingScroll.disableAnimation();
            followingScroll.scrollVertically(scrollOffset);
            followingScroll.enableAnimation();
        }

        private void recordScrollingEditor(Editor activeEditor) {
            scrollingEditor = activeEditor;
            alarm.cancelAllRequests();
            alarm.addRequest(() -> scrollingEditor = null, 300);
        }
    }

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
    public String getEditorTypeId() {
        return myEditorTypeId;
    }

    @NotNull
    @Override
    public FileEditor createEditor(
            @NotNull final Project project,
            @NotNull final VirtualFile file) {
        Builder builder = new Builder(project, file);
        return builder.build();
    }

    private void updateDisplayFolding(Editor mainEditor, Editor displayEditor) {
        String mainText = mainEditor.getDocument().getText();
        String displayText = displayEditor.getDocument().getText();
        FoldRegion[] mainRegions =
                mainEditor.getFoldingModel().getAllFoldRegions();
        FoldingModel displayModel = displayEditor.getFoldingModel();
        displayModel.runBatchFoldingOperation(() -> {
            for (FoldRegion region : displayModel.getAllFoldRegions()) {
                displayModel.removeFoldRegion(region);
            }
            for (FoldRegion mainRegion : mainRegions) {
                if (!mainRegion.isExpanded()) {
                    int startOffset = mainRegion.getStartOffset();
                    int endOffset = mainRegion.getEndOffset();
                    int startLine = StringUtil.offsetToLineNumber(
                            mainText,
                            startOffset);
                    int endLine = StringUtil.offsetToLineNumber(
                            mainText,
                            endOffset);
                    int startFold = StringUtil.lineColToOffset(
                            displayText,
                            startLine,
                            0);
                    int endFold = StringUtil.lineColToOffset(
                            displayText,
                            endLine,
                            0);
                    if (startFold < endFold) {
                        FoldRegion displayRegion = displayModel.addFoldRegion(
                                startFold,
                                endFold,
                                "...");
                        if (displayRegion != null) {
                            displayRegion.setExpanded(false);
                        }
                    }
                }
            }
        });
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
        }
        else {
            layoutName = null;
        }

        return new SplitFileEditor.MyFileEditorState(layoutName, firstState, secondState);
    }

    @Override
    public void writeState(@NotNull FileEditorState state, @NotNull Project project, @NotNull Element targetElement) {
        if (!(state instanceof SplitFileEditor.MyFileEditorState)) {
            return;
        }
        final SplitFileEditor.MyFileEditorState compositeState = (SplitFileEditor.MyFileEditorState)state;

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

    @NotNull
    @Override
    public FileEditorPolicy getPolicy() {
        return FileEditorPolicy.PLACE_BEFORE_DEFAULT_EDITOR;
    }
}

