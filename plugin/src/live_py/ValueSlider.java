package live_py;

import org.eclipse.swt.graphics.Rectangle;

public class ValueSlider {
	private ValueSource source;
	private Rectangle bounds;
	private int startY;
	boolean isSliding;
	private int startValue;
	
	public ValueSlider(ValueSource source, Rectangle bounds) {
		this.source = source;
		this.bounds = bounds;
	}

	public void update(int x, int y, boolean isPressed) {
		if (isSliding) {
			double offset = startY - y;
			double newValue = 
					startValue * (1 + Math.pow(offset, 3) /
					Math.pow(bounds.height/2, 3) * 20);
			source.replaceSelectedText(String.format("%1$.0f", newValue));
		}
		else {
			try
			{
				startValue = Integer.parseInt(source.getSelectedText());
				isSliding = true;
				startY = y;
			}
			catch (NumberFormatException e) {
				// ignore it, just don't start sliding.
			}
		}
	}
}
