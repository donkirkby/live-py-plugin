package live_py;

import java.io.BufferedReader;
import java.io.StringReader;

import org.junit.Assert;
import org.junit.Test;

import io.github.donkirkby.livecanvas.CanvasCommand;
import io.github.donkirkby.livecanvas.CanvasCommand.FontOptions;
import io.github.donkirkby.livecanvas.CanvasReader;

public class CanvasReaderTest {

	@Test
	public void onlyXAndY() {
		// SETUP
		String input = "create_line\n    100\n    200\n    110\n    210\n";
		BufferedReader reader = new BufferedReader(new StringReader(input));
		
		// EXEC
		CanvasReader canvasReader = new CanvasReader(reader);
		CanvasCommand command1 = canvasReader.read();
		CanvasCommand command2 = canvasReader.read();
		canvasReader.close();
		
		// VERIFY
		Assert.assertNotNull("first command should be valid", command1);
		Assert.assertEquals("command", "create_line", command1.getName());
		Assert.assertEquals(
				"coordinate count", 
				4, 
				command1.getCoordinateCount());
		Assert.assertEquals("x1", 100, command1.getCoordinate(0));
		Assert.assertEquals("y2", 210, command1.getCoordinate(3));
		Assert.assertNull("no second command expected", command2);
	}

	@Test
	public void options() {
		// SETUP
		String input = "create_line\n    100\n    200\n    anchor='SW'\n";
		BufferedReader reader = new BufferedReader(new StringReader(input));
		
		// EXEC
		CanvasReader canvasReader = new CanvasReader(reader);
		CanvasCommand command1 = canvasReader.read();
		CanvasCommand command2 = canvasReader.read();
		canvasReader.close();
		
		// VERIFY
		Assert.assertNotNull("first command should be valid", command1);
		Assert.assertEquals("anchor", "SW", command1.getOption("anchor"));
		Assert.assertNull("no second command expected", command2);
	}
	
	@Test
	public void fontOption() {
		// SETUP
		String input = "create_line\n    font=('Arial', 8, 'normal')\n";
		BufferedReader reader = new BufferedReader(new StringReader(input));
		
		// EXEC
		CanvasReader canvasReader = new CanvasReader(reader);
		CanvasCommand command = canvasReader.read();
		FontOptions font = command.getFontOptions("font");
		
		// VERIFY
		Assert.assertNotNull("font should be valid", font);
		Assert.assertEquals("font name", "Arial", font.getName());
	}
}
