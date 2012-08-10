package live_py;

import junit.framework.Assert;

import org.eclipse.swt.graphics.Rectangle;
import org.junit.Test;

public class ValueSliderTest {

	@Test
	public void test() {
		// SETUP
		ValueSourceStub source = new ValueSourceStub();
		source.replaceSelectedText("100");
		
		Rectangle bounds = new Rectangle(0, 0, 100, 100);
		
		// EXEC
		ValueSlider slider = new ValueSlider(source, bounds);
		
		// arguments are: x, y, isPressed
		slider.update(0, 50, true); // start slide
		slider.update(0, 25, true);
		
		String updated = source.getSelectedText();
		
		// VERIFY
		Assert.assertEquals("updated text", "350", updated);
	}

	private class ValueSourceStub implements ValueSource {
		private String selectedText;
		
		@Override
		public String getSelectedText() {
			return selectedText;
		}

		@Override
		public void replaceSelectedText(String text) {
			selectedText = text;
		}
		
	}
}
