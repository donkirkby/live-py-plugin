package io.github.donkirkby.livepycharm;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.*;
import com.intellij.openapi.editor.event.*;
import com.intellij.openapi.fileEditor.*;
import com.intellij.openapi.fileEditor.impl.text.PsiAwareTextEditorProvider;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.openapi.progress.CoroutinesKt;
import com.intellij.openapi.progress.TasksKt;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.util.Alarm;
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
        FileEditor first = myFirstProvider.createEditor(project, file);
        FileEditor second = mySecondProvider.createEditor(project, file);
        LightVirtualFile displayFile = new LightVirtualFile(
                file.getName(),
                FileTypes.PLAIN_TEXT,
                "created for " + file.getName() + "\n");
        FileDocumentManager documentManager = FileDocumentManager.getInstance();
//        Document mainDocument = ApplicationManager.getApplication().runReadAction(
//                (Computable<Document>) () -> documentManager.getDocument(file));
        Document displayDocument = ApplicationManager.getApplication().runReadAction(
                (Computable<Document>) () -> documentManager.getDocument(displayFile));
        return createSplitEditor(first, second, displayFile, displayDocument);
    }

    @NotNull
    @Override
    public String getEditorTypeId() {
        return myEditorTypeId;
    }

//    @NotNull
//    @Override
//    public Builder createEditorAsync(
//            @NotNull final Project project,
//            @NotNull final VirtualFile file) {
//        LightVirtualFile displayFile = new LightVirtualFile(
//                file.getName(),
//                FileTypes.PLAIN_TEXT,
//                "created for " + file.getName() + "\n");
//        FileDocumentManager documentManager = FileDocumentManager.getInstance();
//        Document mainDocument = ApplicationManager.getApplication().runReadAction(
//                (Computable<Document>) () -> documentManager.getDocument(file));
//        Document displayDocument = ApplicationManager.getApplication().runReadAction(
//                (Computable<Document>) () -> documentManager.getDocument(displayFile));
//        final Builder firstBuilder =
//                getBuilderFromEditorProvider(myFirstProvider, project, file);
//        final Builder secondBuilder =
//                getBuilderFromEditorProvider(mySecondProvider, project, displayFile);
//
//        return new Builder() {
//            private Editor mainEditor;
//            private Editor displayEditor;
//            private Editor scrollingEditor;
//            private int displayX;
//            private final Alarm alarm = new Alarm();
//
//            @Override
//            public FileEditor build() {
//                Disposable disposable = () -> {
//                };
//                EditorFactory.getInstance().addEditorFactoryListener(
//                        new EditorFactoryListener() {
//                            @Override
//                            public void editorCreated(@NotNull EditorFactoryEvent event) {
//                                Editor editor = event.getEditor();
//                                Document document = editor.getDocument();
//                                if (mainEditor == null &&
//                                        document == mainDocument) {
//                                    mainEditor = editor;
//                                } else if (displayEditor == null &&
//                                        document == displayDocument) {
//                                    displayEditor = editor;
//                                }
//                            }
//                        },
//                        disposable
//                );
//                FileEditor editor = createSplitEditor(
//                        firstBuilder.build(),
//                        secondBuilder.build(),
//                        file,
//                        displayDocument);
//                Editor mainEditor = this.mainEditor;
//                Editor displayEditor = this.displayEditor;
//                if (mainEditor != null && displayEditor != null) {
//                    mainEditor.getScrollingModel().addVisibleAreaListener(
//                            e -> updateScrolling(mainEditor));
//                    displayEditor.getScrollingModel().addVisibleAreaListener(
//                            e -> updateScrolling(displayEditor));
//
//                    if (displayDocument != null) {
//                        displayDocument.addDocumentListener(new DocumentListener() {
//                            @Override
//                            public void documentChanged(@NotNull DocumentEvent event) {
//                                updateDisplayFolding(mainEditor, displayEditor);
//                            }
//                        });
//                    }
//                }
//                return editor;
//            }
//
//            private void updateScrolling(Editor activeEditor) {
//                // Horizontal scroll remembers manually scrolled position.
//                SplitFileEditor splitFileEditor =
//                        SplitFileEditor.getSplitFileEditor(mainEditor);
//                ScrollingModel displayScroll =
//                        displayEditor.getScrollingModel();
//
//                boolean isUpdating = splitFileEditor != null &&
//                        splitFileEditor.isDisplayUpdating();
//                boolean isDisplayHidden = splitFileEditor != null &&
//                        splitFileEditor.getLayout() !=
//                                SplitFileEditor.SplitEditorLayout.DISPLAY;
//                if (isUpdating) {
//                    displayScroll.disableAnimation();
//                    displayScroll.scrollHorizontally(displayX);
//                    displayScroll.enableAnimation();
//                } else {
//                    displayX = displayScroll.getVisibleArea().x;
//                }
//
//                // Vertical scroll synchronized between two sides.
//                Editor slaveEditor;
//                if (scrollingEditor != null) {
//                    activeEditor = scrollingEditor;
//                    slaveEditor = activeEditor == mainEditor
//                            ? displayEditor
//                            : mainEditor;
//                } else if (activeEditor == mainEditor || isUpdating || isDisplayHidden) {
//                    activeEditor = mainEditor;
//                    slaveEditor = displayEditor;
//                } else {
//                    slaveEditor = mainEditor;
//                }
//                recordScrollingEditor(activeEditor);
//
//                ScrollingModel masterScroll = activeEditor.getScrollingModel();
//                ScrollingModel slaveScroll = slaveEditor.getScrollingModel();
//                int scrollOffset = masterScroll.getVisibleArea().y;
//                slaveScroll.disableAnimation();
//                slaveScroll.scrollVertically(scrollOffset);
//                slaveScroll.enableAnimation();
//            }
//
//            private void recordScrollingEditor(Editor activeEditor) {
//                scrollingEditor = activeEditor;
//                alarm.cancelAllRequests();
//                alarm.addRequest(() -> scrollingEditor = null, 300);
//            }
//        };
//    }
//
//    private void updateDisplayFolding(Editor mainEditor, Editor displayEditor) {
//        String mainText = mainEditor.getDocument().getText();
//        String displayText = displayEditor.getDocument().getText();
//        FoldRegion[] mainRegions =
//                mainEditor.getFoldingModel().getAllFoldRegions();
//        FoldingModel displayModel = displayEditor.getFoldingModel();
//        displayModel.runBatchFoldingOperation(() -> {
//            for (FoldRegion region : displayModel.getAllFoldRegions()) {
//                displayModel.removeFoldRegion(region);
//            }
//            for (FoldRegion mainRegion : mainRegions) {
//                if (!mainRegion.isExpanded()) {
//                    int startOffset = mainRegion.getStartOffset();
//                    int endOffset = mainRegion.getEndOffset();
//                    int startLine = StringUtil.offsetToLineNumber(
//                            mainText,
//                            startOffset);
//                    int endLine = StringUtil.offsetToLineNumber(
//                            mainText,
//                            endOffset);
//                    int startFold = StringUtil.lineColToOffset(
//                            displayText,
//                            startLine,
//                            0);
//                    int endFold = StringUtil.lineColToOffset(
//                            displayText,
//                            endLine,
//                            0);
//                    if (startFold < endFold) {
//                        FoldRegion displayRegion = displayModel.addFoldRegion(
//                                startFold,
//                                endFold,
//                                "...");
//                        if (displayRegion != null) {
//                            displayRegion.setExpanded(false);
//                        }
//                    }
//                }
//            }
//        });
//    }

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

//    @NotNull
//    private static Builder getBuilderFromEditorProvider(@NotNull final com.intellij.openapi.fileEditor.FileEditorProvider provider,
//                                                        @NotNull final Project project,
//                                                        @NotNull final VirtualFile file) {
//        if ( ! (provider instanceof AsyncFileEditorProvider)) {
//            return new Builder() {
//                @Override
//                public @NotNull FileEditor build() {
//                    return provider.createEditor(project, file);
//                }
//            };
//        }
//
//        // called with write context
//        AsyncFileEditorProvider asyncProvider = (AsyncFileEditorProvider) provider;
//        if (runsInProjectView() || runsInEventDispatchThread()) {
//            return CoroutinesKt.runBlockingCancellable(
//                    (coroutineScope, continuation) -> asyncProvider.createEditorBuilder(
//                            project,
//                            file,
//                            null,
//                            continuation));
//        }
//        return TasksKt.runWithModalProgressBlocking(
//                project,
//                "Opening " + file.getName(),
//                ((coroutineScope, continuation) -> (
//                        asyncProvider.createEditorBuilder(
//                                project,
//                                file,
//                                null,
//                                continuation))));
//    }
}

