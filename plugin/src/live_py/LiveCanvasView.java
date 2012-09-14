package live_py;

import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.python.pydev.editor.PyEdit;

public class LiveCanvasView extends ViewPart {
	private LiveCodingAnalyst analyst;
	private Canvas canvas;
	private HashMap<String, Color> colorMap = new HashMap<String, Color>();
	
	public LiveCanvasView() {
		super();
	}

	public void setFocus() {
	}
	
	public void createPartControl(Composite parent) {
		canvas = new Canvas(parent, SWT.NONE);
 		
		canvas.addPaintListener(new PaintListener() {
			
			@Override
			public void paintControl(PaintEvent e) {
				drawResult(e.gc);
			}
		});
		
		parent.addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(ControlEvent e) {
				if (analyst != null) {
					analyst.refresh();
				}
			}
		});
		
		IViewSite site = getViewSite();
		site.getPage().addPartListener(new IPartListener() {
			
			@Override
			public void partOpened(IWorkbenchPart part) {
			}
			
			@Override
			public void partDeactivated(IWorkbenchPart part) {
			}
			
			@Override
			public void partClosed(IWorkbenchPart part) {
			}
			
			@Override
			public void partBroughtToTop(IWorkbenchPart part) {
			}
			
			@Override
			public void partActivated(IWorkbenchPart part) {
				if (part instanceof PyEdit)
				{
					PyEdit editor = (PyEdit)part;
					LiveCodingAnalyst newAnalyst = 
							LiveCodingAnalyst.getAnalyst(editor);
					if (newAnalyst != analyst && analyst != null) {
						analyst.setCanvasView(null);
					}
					analyst = newAnalyst;
					analyst.setCanvasView(LiveCanvasView.this);
				}
			}
		});
	}
	
	public void redraw() {
		if (canvas != null) {
			canvas.redraw();
		}
	}
	
	public Rectangle getBounds() {
		return
				canvas == null
				? null
				: canvas.getBounds();
	}
	
	private void drawResult(GC gc) {
		// Clear the drawing
		gc.fillRectangle(gc.getDevice().getBounds());
		
		if (analyst == null) {
			return;
		}
		ArrayList<CanvasCommand> canvasCommands = analyst.getCanvasCommands();
		// Execute the drawing commands
		for (CanvasCommand command : canvasCommands) {
			String method = command.getName();
			String fill = command.getOption("fill");
			Color oldForeground = gc.getForeground();
			Color newForeground = null;
			if (fill != null) {
				newForeground = getColor(fill);
				if (newForeground != null) {
					gc.setForeground(newForeground);
				}
			}
			if (method.equals("create_line")) {
				gc.drawLine(
						command.getCoordinate(0),
						command.getCoordinate(1),
						command.getCoordinate(2),
						command.getCoordinate(3));
			}
			else if (method.equals("create_rectangle")) {
				gc.drawRectangle(
						command.getCoordinate(0),
						command.getCoordinate(1),
						command.getCoordinate(2) - command.getCoordinate(0),
						command.getCoordinate(3) - command.getCoordinate(1));
			}
			else if (method.equals("create_text")) {
				Font oldFont = gc.getFont();
				gc.setFont(command.getFontOption(gc.getDevice(), "font"));
				String text = command.getOption("text");
				Point size = gc.textExtent(text);
				int x = command.getCoordinate(0);
				int y = command.getCoordinate(1);
				String anchor = command.getOption("anchor");
				anchor = anchor == null ? "center" : anchor;
				if (anchor.startsWith("s")) {
					y -= size.y;
				}
				else if (anchor.startsWith("n")) {
					// defaults to top
				}
				else {
					y -= size.y/2;
				}
				if (anchor.endsWith("e")) {
					x -= size.x;
				}
				else if (anchor.endsWith("w")) {
					// defaults to left side
				}
				else {
					x -= size.x/2;
				}
				gc.drawText(
						text, 
						x, 
						y,
						SWT.DRAW_TRANSPARENT);
				gc.setFont(oldFont);
			}
			if (newForeground != null)
			{
				gc.setForeground(oldForeground);
			}
		}
		disposeColors();
	}

	private void disposeColors() {
		for (Color color : colorMap.values()) {
			color.dispose();
		}
		colorMap.clear();
	}

	private Color getColor(String fill) {
		Color newForeground;
		newForeground = colorMap.get(fill);
		if (newForeground == null && fill.startsWith("#")) {
			int colorInt = Integer.parseInt(fill.substring(1), 16);
			int red = (colorInt >> 16) % 256;
			int green = (colorInt >> 8) % 256;
			int blue = colorInt % 256;
			newForeground = new Color(Display.getCurrent(), red, green, blue);
			colorMap.put(fill, newForeground);
		}
		return newForeground;
	}
	
}
