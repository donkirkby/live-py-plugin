package live_py;

import org.junit.Assert;
import org.junit.Test;

public class ValueSliderTest {

	@Test
	public void test() {
		// SETUP
		ValueSourceStub source = new ValueSourceStub();
		source.replaceSelectedText("100");
		
		int x = 0;
		int y = 0;
		int width = 100;
		int height = 100;

		// EXEC
		ValueSlider slider = new ValueSlider(source, x, y, width, height);
		
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
