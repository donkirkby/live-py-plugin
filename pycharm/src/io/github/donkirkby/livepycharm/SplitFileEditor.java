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
import com.intellij.ui.JBColor;
import com.intellij.ui.JBSplitter;
import com.intellij.util.ui.UIUtil;
import io.github.donkirkby.livecanvas.CanvasCommand;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.util.Base64;
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
    private final TurtleCanvas turtleCanvas;
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
                turtleCanvas);

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
        private boolean isComparing;

        @Override
        public void setCommands(List<CanvasCommand> commands) {
            this.canvasCommands = commands;
            repaint();
        }

        private Color getColor(String fill) {
            int rgb;
            if ( fill == null || ! fill.startsWith("#")) {
                rgb = 0;
            }
            else {
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
            } else if ( ! anchor.startsWith("n")) {
                y -= textSize.getHeight() / 2;
            } // else defaults to top

            if (anchor.endsWith("e")) {
                x -= textSize.getWidth();
            } else if ( ! anchor.endsWith("w")) {
                x -= textSize.getWidth() / 2;
            } // else defaults to left side

            drawText(graphics, text, x, y, true);
            graphics.setFont(oldFont);
        }

        @Override
        public void paint(Graphics graphics)
        {
            Graphics2D gc = (Graphics2D) graphics;
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

            if (canvasCommands == null || canvasCommands.size() == 0) {
                CanvasCommand command = new CanvasCommand();
                command.addCoordinate(bounds.width/2);
                command.addCoordinate(bounds.height/2);
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
                drawText(graphics, command);
                return;
            }
            if (isComparing() && paintComparison(graphics)) {
                return;
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
                }
                else {
                    newForeground = getColor(fill);
                }
                graphics.setColor(newForeground);
                if (newLineWidthText != null) {
                    Stroke newStroke = new BasicStroke(
                            (int)Math.round(Double.parseDouble(newLineWidthText)),
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

        private boolean paintComparison(Graphics graphics) {
            BufferedImage currentImage = null;
            BufferedImage targetImage = null;
            int currentX = 0;
            int currentY = 0;
            int targetY = 0;
            for (CanvasCommand command : canvasCommands) {
                if (CanvasCommand.CREATE_IMAGE.equals(command.getName())) {
                    if (currentImage == null) {
                        currentImage = readImage(command);
                        currentX = command.getCoordinate(0);
                        currentY = command.getCoordinate(1);
                    } else {
                        targetImage = readImage(command);
                        targetY = command.getCoordinate(1);
                    }
                }
            }
            if (targetImage == null) {
                return false;
            }
            int width = currentImage.getWidth() > targetImage.getWidth()
                    ? currentImage.getWidth()
                    : targetImage.getWidth();
            int height = currentImage.getHeight() > targetImage.getHeight()
                    ? currentImage.getHeight()
                    : targetImage.getHeight();
            BufferedImage diffImage = UIUtil.createImage(
                    width,
                    height,
                    BufferedImage.TYPE_INT_RGB);
            for (int x = 0; x < width; x++) {
                for (int y = 0; y < height; y++) {
                    int currentRGB = getRGB(currentImage, x, y);
                    int targetRGB = getRGB(targetImage, x, y);
                    int diff = 0;
                    for (int byteNum = 0; byteNum < 3; byteNum++) {
                        int currentByte = currentRGB % 256;
                        int targetByte = targetRGB % 256;
                        diff += currentByte < targetByte
                                ? targetByte - currentByte
                                : currentByte - targetByte;
                        currentRGB >>= 8;
                        targetRGB >>= 8;
                    }
                    diff /= 3;  // Average diff of the three channels.
                    int diffRGB = (((diff << 8) + diff) << 8) + diff;
                    diffImage.setRGB(x, y, diffRGB);
                }
            }

            graphics.drawImage(
                    diffImage,
                    currentX,
                    (currentY + targetY) / 2,
                    null);
            return true;
        }

        private int getRGB(BufferedImage currentImage, int x, int y) {
            if (x >= currentImage.getWidth() || y >= currentImage.getHeight()) {
                return 0;
            }
            int rgb = currentImage.getRGB(x, y);
            rgb &= (1 << 24) - 1;
            return rgb;
        }

        private Font getFontOption(CanvasCommand command) {
            CanvasCommand.FontOptions fontOptions =
                    command.getFontOptions("font");
            int style = Font.PLAIN;
            for (String styleName : fontOptions.getStyleNames()) {
                if (styleName.equals("bold")) {
                    style |= Font.BOLD;
                }
                else if (styleName.equals("italic")) {
                    style |= Font.ITALIC;
                }
            }
            return new Font(fontOptions.getName(), style, fontOptions.getSize()*4/3 - 1);

        }

        boolean isComparing() {
            return isComparing;
        }

        void setComparing(boolean comparing) {
            isComparing = comparing;
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
        splitter.setFirstComponent(myMainEditor.getComponent());
        displayCards = new JPanel(new CardLayout());
        displayCards.add(mySecondEditor.getComponent(), SplitEditorLayout.DISPLAY.toString());
        displayCards.add(turtleCanvas, SplitEditorLayout.TURTLE.toString());
        splitter.setSecondComponent(displayCards);

        turtleCanvas.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent componentEvent) {
                super.componentResized(componentEvent);
                myAnalyst.schedule();
            }
        });
        turtleCanvas.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent mouseEvent) {
                super.mouseEntered(mouseEvent);
                turtleCanvas.setComparing(true);
                turtleCanvas.repaint();
            }

            @Override
            public void mouseExited(MouseEvent mouseEvent) {
                super.mouseExited(mouseEvent);
                turtleCanvas.setComparing(false);
                turtleCanvas.repaint();
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

        mySplitEditorLayout = newLayout;
        invalidateLayout();
    }

    void startAnalysis(@Nullable Project project, @NotNull DataContext dataContext) {
        if (myAnalyst.start(project, dataContext)){
            triggerLayoutChange(SplitEditorLayout.DISPLAY);
        }
    }

    boolean isAnalysisPassing() {
        return myAnalyst.isPassing();
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
        // We always want the editor to open without live display, so only save
        // splitter layout during navigation.
        String splitLayout =
                level == FileEditorStateLevel.NAVIGATION
                        ? mySplitEditorLayout.name()
                        : null;
        return new MyFileEditorState(
                splitLayout,
                myMainEditor.getState(level),
                mySecondEditor.getState(level));
    }

    @NotNull
    SplitEditorLayout getLayout() {
        return mySplitEditorLayout;
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
