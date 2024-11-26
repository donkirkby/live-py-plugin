package io.github.donkirkby.livepycharm;

import com.intellij.codeHighlighting.BackgroundEditorHighlighter;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ScrollingModel;
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
import com.intellij.ui.JBColor;
import com.intellij.ui.JBSplitter;
import com.intellij.util.Alarm;
import io.github.donkirkby.livecanvas.CanvasCommand;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.util.*;
import java.util.List;

public class SplitFileEditor extends UserDataHolderBase implements TextEditor {
    private static final Key<SplitFileEditor> PARENT_SPLIT_KEY = Key.create("parentSplit");

    private static final String MY_PROPORTION_KEY = "SplitFileEditor.Proportion";

    @NotNull
    private final FileEditor mainFileEditor;
    @NotNull
    private final FileEditor displayFileEditor;
    @NotNull
    private final JComponent myComponent;
    @NotNull
    private final TurtleCanvas turtleCanvas;
    @NotNull
    private SplitEditorLayout mySplitEditorLayout = SplitEditorLayout.SINGLE;
    private final LiveCodingAnalyst myAnalyst;

    // Scrolling controls
    private boolean isScrollingRegistered = false;
    private int displayX;
    private Editor scrollingEditor;
    private final Alarm alarm = new Alarm();

    @NotNull
    private final MyListenersMultimap myListenersGenerator = new MyListenersMultimap();
    private JPanel displayCards;

    SplitFileEditor(@NotNull FileEditor mainEditor,
                    @NotNull FileEditor secondEditor,
                    VirtualFile file,
                    Document displayDocument) {
        mainFileEditor = mainEditor;
        displayFileEditor = secondEditor;
        turtleCanvas = new TurtleCanvas();
        myAnalyst = new LiveCodingAnalyst(
                file,
                displayDocument,
                this,
                turtleCanvas);

        myComponent = createComponent();

        if (mainFileEditor instanceof TextEditor) {
            mainFileEditor.putUserData(PARENT_SPLIT_KEY, this);
        }
        if (displayFileEditor instanceof TextEditor) {
            displayFileEditor.putUserData(PARENT_SPLIT_KEY, this);
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

        private Color getColor(String fill) {
            int rgb;
            if (fill == null || !fill.startsWith("#")) {
                rgb = 0;
            } else {
                rgb = Integer.parseInt(fill.substring(1), 16);
            }
            return new JBColor(rgb, rgb);
        }

        private Dimension drawText(
                Graphics graphics,
                String text,
                int x,
                int y,
                boolean isDrawing) {
            FontMetrics fontMetrics = graphics.getFontMetrics();
            Rectangle2D tabBounds = fontMetrics.getStringBounds(
                    "xxxxxxxx",
                    graphics);
            int tabWidth = (int) tabBounds.getWidth();
            int height = 0, maxWidth = 0;
            String[] lines = text.split("\n");
            for (String line : lines) {
                int width = 0;
                String[] columns = line.split("\t");
                for (String column : columns) {
                    if (width != 0) {
                        // Align to tab stop.
                        width += tabWidth - width % tabWidth;
                    }
                    Rectangle2D columnBounds = fontMetrics.getStringBounds(
                            column,
                            graphics);
                    if (isDrawing) {
                        graphics.drawString(
                                column,
                                x + width,
                                y + height + fontMetrics.getAscent());
                    }
                    width += columnBounds.getWidth();
                }
                height += fontMetrics.getHeight();
                maxWidth = Math.max(maxWidth, width);
            }
            return new Dimension(maxWidth, height);
        }

        private void drawImage(Graphics graphics, CanvasCommand command) {
            BufferedImage image = readImage(command);
            graphics.drawImage(
                    image,
                    command.getCoordinate(0),
                    command.getCoordinate(1),
                    null);
        }

        private BufferedImage readImage(CanvasCommand command) {
            byte[] decoded =
                    Base64.getDecoder().decode(command.getOption("image"));
            BufferedImage image;
            try {
                try (ByteArrayInputStream inputStream = new ByteArrayInputStream(decoded)) {
                    image = ImageIO.read(inputStream);
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            return image;
        }

        private void drawText(Graphics graphics, CanvasCommand command) {
            Font oldFont = graphics.getFont();
            Font font = getFontOption(command);
            graphics.setFont(font);
            int x = command.getCoordinate(0);
            int y = command.getCoordinate(1);
            String text = command.getOption("text");
            Dimension textSize = drawText(graphics, text, x, y, false);
            String anchor = command.getOption("anchor");
            anchor = anchor == null ? "center" : anchor;
            if (anchor.startsWith("s")) {
                y -= textSize.getHeight();
            } else if (!anchor.startsWith("n")) {
                y -= textSize.getHeight() / 2;
            } // else defaults to top

            if (anchor.endsWith("e")) {
                x -= textSize.getWidth();
            } else if (!anchor.endsWith("w")) {
                x -= textSize.getWidth() / 2;
            } // else defaults to left side

            drawText(graphics, text, x, y, true);
            graphics.setFont(oldFont);
        }

        @Override
        public void paint(Graphics graphics) {
            Graphics2D gc = (Graphics2D) graphics;
            EditorImpl editor;
            try {
                editor = myAnalyst.getEditor();
            } catch (RuntimeException ex) {
                editor = null;
            }
            if (editor != null) {
                graphics.setFont(editor.getColorsScheme().getFont(
                        EditorFontType.PLAIN));
            }
            Rectangle bounds = getBounds();
            graphics.clearRect(
                    bounds.x,
                    bounds.y,
                    bounds.width,
                    bounds.height);

            if (canvasCommands == null || canvasCommands.size() == 0) {
                CanvasCommand command = new CanvasCommand();
                command.setName(CanvasCommand.CREATE_TEXT);
                command.addCoordinate(bounds.width / 2);
                command.addCoordinate(bounds.height / 2);
                command.setOption("font", "('Arial', 12, 'normal')");
                command.setOption(
                        "text",
                        "No turtle or matplotlib commands found.\n" +
                                "A turtle example:\n" +
                                "\n" +
                                "from turtle import *\n" +
                                "forward(100)\n" +
                                "\n" +
                                "A matplotlib example:\n" +
                                "\n" +
                                "import matplotlib.pyplot as plt\n" +
                                "plt.plot([3, 1, 4])\n" +
                                "plt.show()");
                canvasCommands = new ArrayList<>();
                canvasCommands.add(command);
            }
            if (!canvasCommands.get(0).getName().equals(
                    CanvasCommand.BACKGROUND_COLOR)) {
                CanvasCommand command = new CanvasCommand();
                command.setName(CanvasCommand.BACKGROUND_COLOR);
                command.setOption("fill", "#FFFFFF");
                canvasCommands.add(0, command);
            }
            for (CanvasCommand command : canvasCommands) {
                String method = command.getName();
                String fill = command.getOption("fill");
                String outline = command.getOption("outline");
                String newLineWidthText = command.getOption("pensize");
                Color oldColor = graphics.getColor();
                Color newForeground;
                Color newBackground = null;
                Stroke oldStroke = gc.getStroke();
                if (outline != null) {
                    newForeground = getColor(outline);
                    newBackground = getColor(fill);
                } else {
                    newForeground = getColor(fill);
                }
                graphics.setColor(newForeground);
                if (newLineWidthText != null) {
                    Stroke newStroke = new BasicStroke(
                            (int) Math.round(Double.parseDouble(newLineWidthText)),
                            BasicStroke.CAP_ROUND,
                            BasicStroke.JOIN_ROUND);
                    gc.setStroke(newStroke);
                }
                switch (method) {
                    case CanvasCommand.BACKGROUND_COLOR:
                        graphics.setColor(newBackground);
                        graphics.fillRect(
                                bounds.x,
                                bounds.y,
                                bounds.width,
                                bounds.height);
                        break;
                    case CanvasCommand.CREATE_LINE:
                        graphics.drawLine(
                                command.getCoordinate(0),
                                command.getCoordinate(1),
                                command.getCoordinate(2),
                                command.getCoordinate(3));
                        break;
                    case CanvasCommand.CREATE_POLYGON:
                        int[] xCoordinates = command.getXCoordinates();
                        int[] yCoordinates = command.getYCoordinates();

                        if (newBackground != null) {
                            graphics.setColor(newBackground);
                            graphics.fillPolygon(
                                    xCoordinates,
                                    yCoordinates,
                                    xCoordinates.length);
                        }
                        break;
                    case CanvasCommand.CREATE_TEXT:
                        drawText(gc, command);
                        break;
                    case CanvasCommand.CREATE_IMAGE:
                        drawImage(gc, command);
                        break;
                }
                graphics.setColor(oldColor);
                gc.setStroke(oldStroke);
            }
        }

        private Font getFontOption(CanvasCommand command) {
            CanvasCommand.FontOptions fontOptions =
                    command.getFontOptions("font");
            int style = Font.PLAIN;
            for (String styleName : fontOptions.getStyleNames()) {
                if (styleName.equals("bold")) {
                    style |= Font.BOLD;
                } else if (styleName.equals("italic")) {
                    style |= Font.ITALIC;
                }
            }
            return new Font(fontOptions.getName(), style, fontOptions.getSize() * 4 / 3 - 1);

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
        return "Live Coding in Python Split Editor";
    }

    @Override
    public @Nullable VirtualFile getFile() {
        Editor editor = this.getEditor();
        Document document = editor.getDocument();
        return FileDocumentManager.getInstance().getFile(document);
    }

    boolean isDisplayUpdating() {
        return myAnalyst.isDisplayUpdating();
    }

    boolean isRunningSelectedConfiguration(@Nullable Project project) {
        return myAnalyst.isRunningSelectedConfiguration(project);
    }

    @NotNull
    private JComponent createComponent() {
        final JBSplitter splitter = new JBSplitter(false, 0.5f, 0.15f, 0.85f);
        splitter.setSplitterProportionKey(MY_PROPORTION_KEY);
        splitter.setFirstComponent(mainFileEditor.getComponent());
        displayCards = new JPanel(new CardLayout());
        displayCards.add(displayFileEditor.getComponent(), SplitEditorLayout.DISPLAY.toString());
        displayCards.add(turtleCanvas, SplitEditorLayout.TURTLE.toString());
        splitter.setSecondComponent(displayCards);

        turtleCanvas.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent componentEvent) {
                super.componentResized(componentEvent);
                myAnalyst.schedule();
            }
        });

        final JPanel result = new JPanel(new BorderLayout());
        result.add(splitter, BorderLayout.CENTER);
        adjustEditorsVisibility();

        return result;
    }

    private void triggerLayoutChange(@NotNull SplitFileEditor.SplitEditorLayout newLayout) {
        if (mySplitEditorLayout == newLayout) {
            return;
        }

        if ( ! isScrollingRegistered) {
            isScrollingRegistered = true;
            var mainEditor = getEditor();
            var displayEditor = getDisplayEditor();
            mainEditor.getScrollingModel().addVisibleAreaListener(
                    e -> updateScrolling(mainEditor));
            displayEditor.getScrollingModel().addVisibleAreaListener(
                    e -> updateScrolling(displayEditor));
        }

        mySplitEditorLayout = newLayout;
        invalidateLayout();
    }

    private void updateScrolling(Editor activeEditor) {
        // Horizontal scroll remembers manually scrolled position.
        var mainEditor = getEditor();
        var displayEditor = getDisplayEditor();
        ScrollingModel displayScroll =
                displayEditor.getScrollingModel();

        boolean isUpdating = isDisplayUpdating();
        boolean isDisplayHidden = getLayout() !=
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

    void startAnalysis(@Nullable Project project, @NotNull DataContext dataContext) {
        if (myAnalyst.startAnalysis(project, dataContext)) {
            triggerLayoutChange(SplitEditorLayout.DISPLAY);
        }
    }

    boolean isAnalysisPassing() {
        return myAnalyst.isPassing();
    }

    void startTurtle(@Nullable Project project, @NotNull DataContext dataContext) {
        if (myAnalyst.startCanvas(project, dataContext)) {
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
        mainFileEditor.getComponent().setVisible(true);
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
        return mainFileEditor.getPreferredFocusedComponent();
    }

    @NotNull
    @Override
    public FileEditorState getState(@NotNull FileEditorStateLevel level) {
        // We always want the editor to open without live display, so only save
        // splitter layout during navigation.
        String splitLayout =
                level == FileEditorStateLevel.NAVIGATION
                        ? mySplitEditorLayout.name()
                        : null;
        return new MyFileEditorState(
                splitLayout,
                mainFileEditor.getState(level),
                displayFileEditor.getState(level));
    }

    @NotNull
    SplitEditorLayout getLayout() {
        return mySplitEditorLayout;
    }

    @Override
    public void setState(@NotNull FileEditorState state) {
        if (state instanceof MyFileEditorState) {
            final MyFileEditorState compositeState = (MyFileEditorState) state;
            if (compositeState.getFirstState() != null) {
                mainFileEditor.setState(compositeState.getFirstState());
            }
            if (compositeState.getSecondState() != null) {
                displayFileEditor.setState(compositeState.getSecondState());
            }
            if (compositeState.getSplitLayout() != null) {
                try {
                    mySplitEditorLayout = SplitEditorLayout.valueOf(compositeState.getSplitLayout());
                    invalidateLayout();
                } catch (IllegalArgumentException e) {
                    // Probably an old layout that is no longer supported.
                }
            }
        }
    }

    @Override
    public boolean isModified() {
        return mainFileEditor.isModified() || displayFileEditor.isModified();
    }

    @Override
    public boolean isValid() {
        return mainFileEditor.isValid() && displayFileEditor.isValid();
    }

    @Override
    public void selectNotify() {
        mainFileEditor.selectNotify();
        displayFileEditor.selectNotify();
    }

    @Override
    public void deselectNotify() {
        mainFileEditor.deselectNotify();
        displayFileEditor.deselectNotify();
    }

    @Override
    public void addPropertyChangeListener(@NotNull PropertyChangeListener listener) {
        mainFileEditor.addPropertyChangeListener(listener);
        displayFileEditor.addPropertyChangeListener(listener);

        final DoublingEventListenerDelegate delegate = myListenersGenerator.addListenerAndGetDelegate(listener);
        mainFileEditor.addPropertyChangeListener(delegate);
        displayFileEditor.addPropertyChangeListener(delegate);
    }

    @Override
    public void removePropertyChangeListener(@NotNull PropertyChangeListener listener) {
        mainFileEditor.removePropertyChangeListener(listener);
        displayFileEditor.removePropertyChangeListener(listener);

        final DoublingEventListenerDelegate delegate = myListenersGenerator.removeListenerAndGetDelegate(listener);
        if (delegate != null) {
            mainFileEditor.removePropertyChangeListener(delegate);
            displayFileEditor.removePropertyChangeListener(delegate);
        }
    }

    @Nullable
    @Override
    public BackgroundEditorHighlighter getBackgroundHighlighter() {
        return mainFileEditor.getBackgroundHighlighter();
    }

    @Nullable
    @Override
    public FileEditorLocation getCurrentLocation() {
        return mainFileEditor.getCurrentLocation();
    }

    @Nullable
    @Override
    public StructureViewBuilder getStructureViewBuilder() {
        return mainFileEditor.getStructureViewBuilder();
    }

    @Override
    public void dispose() {
        Disposer.dispose(mainFileEditor);
        Disposer.dispose(displayFileEditor);
    }

    @NotNull
    @Override
    public Editor getEditor() {
        TextEditor mainTextEditor = (TextEditor) mainFileEditor;
        return mainTextEditor.getEditor();
    }

    @NotNull
    private Editor getDisplayEditor() {
        TextEditor displayTextEditor = (TextEditor) displayFileEditor;
        return displayTextEditor.getEditor();
    }

    @Override
    public boolean canNavigateTo(@NotNull Navigatable navigatable) {
        TextEditor mainTextEditor = (TextEditor) mainFileEditor;
        return mainTextEditor.canNavigateTo(navigatable);
    }

    @Override
    public void navigateTo(@NotNull Navigatable navigatable) {
        TextEditor mainTextEditor = (TextEditor) mainFileEditor;
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
        public boolean canBeMergedWith(
                @NotNull FileEditorState otherState,
                @NotNull FileEditorStateLevel level) {
            return otherState instanceof MyFileEditorState
                    && (myFirstState == null || myFirstState.canBeMergedWith(((MyFileEditorState) otherState).myFirstState, level))
                    && (mySecondState == null || mySecondState.canBeMergedWith(((MyFileEditorState) otherState).mySecondState, level));
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
            } else {
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
            } else {
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
