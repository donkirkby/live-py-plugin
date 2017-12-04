package io.github.donkirkby.livepycharm;

import com.intellij.codeHighlighting.BackgroundEditorHighlighter;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.colors.EditorFontType;
import com.intellij.openapi.editor.impl.EditorImpl;
import com.intellij.openapi.fileEditor.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.UserDataHolderBase;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.Navigatable;
import com.intellij.ui.JBSplitter;
import io.github.donkirkby.livecanvas.CanvasCommand;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.List;
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
    private final JPanel turtleCanvas;
    @NotNull
    private SplitEditorLayout mySplitEditorLayout = SplitEditorLayout.SINGLE;
    private LiveCodingAnalyst myAnalyst;

    @NotNull
    private final MyListenersMultimap myListenersGenerator = new MyListenersMultimap();
    private JPanel displayCards;

    SplitFileEditor(@NotNull FileEditor mainEditor,
                    @NotNull FileEditor secondEditor,
                    VirtualFile file,
                    Document displayDocument) {
        myMainEditor = mainEditor;
        mySecondEditor = secondEditor;
        turtleCanvas = new TurtleCanvas();
        myAnalyst = new LiveCodingAnalyst(
                file,
                displayDocument,
                this,
                (LiveCodingAnalyst.CanvasPainter) turtleCanvas);

        myComponent = createComponent();

        if (myMainEditor instanceof TextEditor) {
            myMainEditor.putUserData(PARENT_SPLIT_KEY, this);
        }
        if (mySecondEditor instanceof TextEditor) {
            mySecondEditor.putUserData(PARENT_SPLIT_KEY, this);
        }
    }

    private class TurtleCanvas
            extends JPanel
            implements LiveCodingAnalyst.CanvasPainter {
        private List<CanvasCommand> canvasCommands;

        @Override
        public void setCommands(List<CanvasCommand> commands) {
            this.canvasCommands = commands;
            repaint();
        }

        void drawString(Graphics g, String text, int x, int y) {
            for (String line : text.split("\n"))
                g.drawString(line, x, y += g.getFontMetrics().getHeight());
        }

        @Override
        public void paint(Graphics graphics)
        {
            Editor[] editors = EditorFactory.getInstance().getEditors(
                    myAnalyst.getDisplayDocument());
            if (editors.length >= 1) {
                EditorImpl editor = (EditorImpl) editors[0];
                graphics.setFont(editor.getColorsScheme().getFont(
                        EditorFontType.PLAIN));
            }
            Rectangle bounds = getBounds();
            graphics.clearRect(
                    bounds.x,
                    bounds.y,
                    bounds.width,
                    bounds.height);

            String message = null;
            if (canvasCommands == null || canvasCommands.size() == 0) {
                message = "No turtle commands found.\n" +
                        "For example:\n" +
                        "from turtle import *\n" +
                        "forward(100)";
            }
            if (message != null) {
                String[] lines = message.split("\n");
                FontMetrics fontMetrics = graphics.getFontMetrics();
                Rectangle2D extent = fontMetrics.getStringBounds(lines[0], graphics);
                drawString(
                        graphics,
                        message,
                        (bounds.width - (int)extent.getWidth())/2,
                        (bounds.height - (int)extent.getHeight()*4)/2);
                return;
            }
            for (CanvasCommand command : canvasCommands) {
                String method = command.getName();
                if (method.equals("create_line")) {
                    graphics.drawLine(
                            command.getCoordinate(0),
                            command.getCoordinate(1),
                            command.getCoordinate(2),
                            command.getCoordinate(3));
                }
            }
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
        displayCards = new JPanel(new CardLayout());
        displayCards.add(mySecondEditor.getComponent(), SplitEditorLayout.DISPLAY.toString());
        displayCards.add(turtleCanvas, SplitEditorLayout.TURTLE.toString());
        splitter.setSecondComponent(displayCards);

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

    void startAnalysis(@Nullable Project project, @NotNull DataContext dataContext) {
        if (myAnalyst.start(project, dataContext)){
            triggerLayoutChange(SplitEditorLayout.DISPLAY);
        }
    }

    void startTurtle(@Nullable Project project, @NotNull DataContext dataContext) {
        if (myAnalyst.start(project, dataContext)){
            triggerLayoutChange(SplitEditorLayout.TURTLE);
        }
    }

    void stopAnalysis() {
        myAnalyst.stop();
        triggerLayoutChange(SplitFileEditor.SplitEditorLayout.SINGLE);
    }

    private void invalidateLayout() {
        adjustEditorsVisibility();
        myComponent.repaint();
    }

    private void adjustEditorsVisibility() {
        myMainEditor.getComponent().setVisible(true);
        displayCards.setVisible(mySplitEditorLayout.showCards);
        CardLayout cardLayout = (CardLayout) displayCards.getLayout();
        cardLayout.show(displayCards, mySplitEditorLayout.toString());
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
                mySplitEditorLayout = SplitEditorLayout.SINGLE;
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
        SINGLE(false, "Single"),
        DISPLAY(true, "Display"),
        TURTLE(true, "Turtle");

        public final boolean showCards;
        public final String presentationName;

        SplitEditorLayout(boolean showCards,
                          String presentationName) {
            this.showCards = showCards;
            this.presentationName = presentationName;
        }

        @Override
        public String toString() {
            return presentationName;
        }
    }
}
