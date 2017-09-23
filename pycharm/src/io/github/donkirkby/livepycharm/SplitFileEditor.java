package io.github.donkirkby.livepycharm;

import com.intellij.codeHighlighting.BackgroundEditorHighlighter;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.UserDataHolderBase;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.IdeFocusManager;
import com.intellij.pom.Navigatable;
import com.intellij.ui.JBSplitter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;

public class SplitFileEditor extends UserDataHolderBase implements TextEditor {
    private static final Key<SplitFileEditor> PARENT_SPLIT_KEY = Key.create("parentSplit");

    private static final String MY_PROPORTION_KEY = "SplitFileEditor.Proportion";

    @NotNull
    private final FileEditor myMainEditor;
    @NotNull
    private final FileEditor mySecondEditor;
    @NotNull
    private final JComponent myComponent;
    @NotNull
    private SplitEditorLayout mySplitEditorLayout = SplitEditorLayout.FIRST;
    private LiveCodingAnalyst myAnalyst;

    @NotNull
    private final MyListenersMultimap myListenersGenerator = new MyListenersMultimap();

    SplitFileEditor(@NotNull FileEditor mainEditor,
                    @NotNull FileEditor secondEditor,
                    VirtualFile file,
                    Document displayDocument) {
        myMainEditor = mainEditor;
        mySecondEditor = secondEditor;
        myAnalyst = new LiveCodingAnalyst(file, displayDocument, this);

        myComponent = createComponent();

        if (myMainEditor instanceof TextEditor) {
            myMainEditor.putUserData(PARENT_SPLIT_KEY, this);
        }
        if (mySecondEditor instanceof TextEditor) {
            mySecondEditor.putUserData(PARENT_SPLIT_KEY, this);
        }
    }

    @Nullable
    static SplitFileEditor getSplitFileEditor(Editor editor) {
        Project project = editor == null ? null : editor.getProject();
        Document doc = editor == null ? null : editor.getDocument();
        VirtualFile file =
                doc == null
                ? null
                : FileDocumentManager.getInstance().getFile(doc);

        SplitFileEditor splitFileEditor = null;
        if (project != null && file != null) {
            FileEditor selectedEditor = FileEditorManager.getInstance(project).getSelectedEditor(file);
            if (selectedEditor instanceof SplitFileEditor) {
                splitFileEditor = (SplitFileEditor) selectedEditor;
            }
        }
        return splitFileEditor;
    }

    @NotNull
    @Override
    public String getName() {
        return "Live coding in Python split editor";
    }

    @NotNull
    private JComponent createComponent() {
        final JBSplitter splitter = new JBSplitter(false, 0.5f, 0.15f, 0.85f);
        splitter.setSplitterProportionKey(MY_PROPORTION_KEY);
        splitter.setFirstComponent(myMainEditor.getComponent());
        splitter.setSecondComponent(mySecondEditor.getComponent());

        final JPanel result = new JPanel(new BorderLayout());
        result.add(splitter, BorderLayout.CENTER);
        adjustEditorsVisibility();

        return result;
    }

    private void triggerLayoutChange(@NotNull SplitFileEditor.SplitEditorLayout newLayout) {
        if (mySplitEditorLayout == newLayout) {
            return;
        }

        mySplitEditorLayout = newLayout;
        invalidateLayout();
    }

    void startAnalysis(RunConfiguration configuration) {
        myAnalyst.start(configuration);
        triggerLayoutChange(SplitFileEditor.SplitEditorLayout.SPLIT);
    }

    void stopAnalysis() {
        myAnalyst.stop();
        triggerLayoutChange(SplitFileEditor.SplitEditorLayout.FIRST);
    }

    private void invalidateLayout() {
        adjustEditorsVisibility();
        // myToolbarWrapper.refresh();
        myComponent.repaint();

        IdeFocusManager.findInstanceByComponent(myComponent).requestFocus(myComponent, true);
    }

    private void adjustEditorsVisibility() {
        myMainEditor.getComponent().setVisible(mySplitEditorLayout.showFirst);
        mySecondEditor.getComponent().setVisible(mySplitEditorLayout.showSecond);
    }

    @NotNull
    @Override
    public JComponent getComponent() {
        return myComponent;
    }

    @Nullable
    @Override
    public JComponent getPreferredFocusedComponent() {
        return myMainEditor.getPreferredFocusedComponent();
    }

    @NotNull
    @Override
    public FileEditorState getState(@NotNull FileEditorStateLevel level) {
        return new MyFileEditorState(mySplitEditorLayout.name(), myMainEditor.getState(level), mySecondEditor.getState(level));
    }

    @Override
    public void setState(@NotNull FileEditorState state) {
        if (state instanceof MyFileEditorState) {
            final MyFileEditorState compositeState = (MyFileEditorState)state;
            if (compositeState.getFirstState() != null) {
                myMainEditor.setState(compositeState.getFirstState());
            }
            if (compositeState.getSecondState() != null) {
                mySecondEditor.setState(compositeState.getSecondState());
            }
            if (compositeState.getSplitLayout() != null) {
                // We always want the editor to open without live display.
                // mySplitEditorLayout = SplitEditorLayout.valueOf(compositeState.getSplitLayout());
                mySplitEditorLayout = SplitEditorLayout.FIRST;
                invalidateLayout();
            }
        }
    }

    @Override
    public boolean isModified() {
        return myMainEditor.isModified() || mySecondEditor.isModified();
    }

    @Override
    public boolean isValid() {
        return myMainEditor.isValid() && mySecondEditor.isValid();
    }

    @Override
    public void selectNotify() {
        myMainEditor.selectNotify();
        mySecondEditor.selectNotify();
    }

    @Override
    public void deselectNotify() {
        myMainEditor.deselectNotify();
        mySecondEditor.deselectNotify();
    }

    @Override
    public void addPropertyChangeListener(@NotNull PropertyChangeListener listener) {
        myMainEditor.addPropertyChangeListener(listener);
        mySecondEditor.addPropertyChangeListener(listener);

        final DoublingEventListenerDelegate delegate = myListenersGenerator.addListenerAndGetDelegate(listener);
        myMainEditor.addPropertyChangeListener(delegate);
        mySecondEditor.addPropertyChangeListener(delegate);
    }

    @Override
    public void removePropertyChangeListener(@NotNull PropertyChangeListener listener) {
        myMainEditor.removePropertyChangeListener(listener);
        mySecondEditor.removePropertyChangeListener(listener);

        final DoublingEventListenerDelegate delegate = myListenersGenerator.removeListenerAndGetDelegate(listener);
        if (delegate != null) {
            myMainEditor.removePropertyChangeListener(delegate);
            mySecondEditor.removePropertyChangeListener(delegate);
        }
    }

    @Nullable
    @Override
    public BackgroundEditorHighlighter getBackgroundHighlighter() {
        return myMainEditor.getBackgroundHighlighter();
    }

    @Nullable
    @Override
    public FileEditorLocation getCurrentLocation() {
        return myMainEditor.getCurrentLocation();
    }

    @Nullable
    @Override
    public StructureViewBuilder getStructureViewBuilder() {
        return myMainEditor.getStructureViewBuilder();
    }

    @Override
    public void dispose() {
        Disposer.dispose(myMainEditor);
        Disposer.dispose(mySecondEditor);
    }

    @NotNull
    @Override
    public Editor getEditor() {
        TextEditor mainTextEditor = (TextEditor) myMainEditor;
        return mainTextEditor.getEditor();
    }

    @Override
    public boolean canNavigateTo(@NotNull Navigatable navigatable) {
        TextEditor mainTextEditor = (TextEditor) myMainEditor;
        return mainTextEditor.canNavigateTo(navigatable);
    }

    @Override
    public void navigateTo(@NotNull Navigatable navigatable) {
        TextEditor mainTextEditor = (TextEditor) myMainEditor;
        mainTextEditor.navigateTo(navigatable);
    }

    static class MyFileEditorState implements FileEditorState {
        @Nullable
        private final String mySplitLayout;
        @Nullable
        private final FileEditorState myFirstState;
        @Nullable
        private final FileEditorState mySecondState;

        MyFileEditorState(@Nullable String splitLayout, @Nullable FileEditorState firstState, @Nullable FileEditorState secondState) {
            mySplitLayout = splitLayout;
            myFirstState = firstState;
            mySecondState = secondState;
        }

        @Nullable
        String getSplitLayout() {
            return mySplitLayout;
        }

        @Nullable
        FileEditorState getFirstState() {
            return myFirstState;
        }

        @Nullable
        FileEditorState getSecondState() {
            return mySecondState;
        }

        @Override
        public boolean canBeMergedWith(FileEditorState otherState, FileEditorStateLevel level) {
            return otherState instanceof MyFileEditorState
                    && (myFirstState == null || myFirstState.canBeMergedWith(((MyFileEditorState)otherState).myFirstState, level))
                    && (mySecondState == null || mySecondState.canBeMergedWith(((MyFileEditorState)otherState).mySecondState, level));
        }
    }

    private class DoublingEventListenerDelegate implements PropertyChangeListener {
        @NotNull
        private final PropertyChangeListener myDelegate;

        private DoublingEventListenerDelegate(@NotNull PropertyChangeListener delegate) {
            myDelegate = delegate;
        }

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            myDelegate.propertyChange(new PropertyChangeEvent(SplitFileEditor.this, evt.getPropertyName(), evt.getOldValue(), evt.getNewValue()));
        }
    }

    private class MyListenersMultimap {
        private final Map<PropertyChangeListener, Pair<Integer, DoublingEventListenerDelegate>> myMap =
                new HashMap<>();

        @NotNull
        DoublingEventListenerDelegate addListenerAndGetDelegate(@NotNull PropertyChangeListener listener) {
            if (!myMap.containsKey(listener)) {
                myMap.put(listener, Pair.create(1, new DoublingEventListenerDelegate(listener)));
            }
            else {
                final Pair<Integer, DoublingEventListenerDelegate> oldPair = myMap.get(listener);
                myMap.put(listener, Pair.create(oldPair.getFirst() + 1, oldPair.getSecond()));
            }

            return myMap.get(listener).getSecond();
        }

        @Nullable
        DoublingEventListenerDelegate removeListenerAndGetDelegate(@NotNull PropertyChangeListener listener) {
            final Pair<Integer, DoublingEventListenerDelegate> oldPair = myMap.get(listener);
            if (oldPair == null) {
                return null;
            }

            if (oldPair.getFirst() == 1) {
                myMap.remove(listener);
            }
            else {
                myMap.put(listener, Pair.create(oldPair.getFirst() - 1, oldPair.getSecond()));
            }
            return oldPair.getSecond();
        }
    }

    public enum SplitEditorLayout {
        FIRST(true, false, "X"),
        SPLIT(true, true, "Z");

        public final boolean showFirst;
        public final boolean showSecond;
        public final String presentationName;

        SplitEditorLayout(boolean showFirst, boolean showSecond, String presentationName) {
            this.showFirst = showFirst;
            this.showSecond = showSecond;
            this.presentationName = presentationName;
        }

        @Override
        public String toString() {
            return presentationName;
        }
    }
}
