package live_py;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;

public class CanvasCommand {
	private String name;
	private ArrayList<Integer> coordinates = 
			new ArrayList<Integer>();
	private	Hashtable<String, String> options = 
			new Hashtable<String, String>();

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	public String getOption(String name) {
		return options.get(name);
	}
	
	public void setOption(String name, String value) {
		options.put(name, value);
	}
	public void addCoordinate(int value) {
		coordinates.add(value);
	}
	public int getCoordinateCount() {
		return coordinates.size();
	}
	public int getCoordinate(int index) {
		return coordinates.get(index);
	}
	public Font getFontOption(Device device, String name) {
		String value = options.get(name);
		if (value == null) {
			return null;
		}
		Pattern pattern = Pattern.compile("\\('([^']+)', (\\d+), '([^']+)'\\)");
		Matcher matcher = pattern.matcher(value);
		if ( ! matcher.matches()) {
			return null;
		}
		String fontName = matcher.group(1);
		int size = Integer.parseInt(matcher.group(2));
		String[] styleNames = matcher.group(3).split(" ");
		int style = SWT.NORMAL;
		for (String styleName : styleNames) {
			if (styleName.equals("bold")) {
				style += SWT.BOLD;
			}
			else if (styleName.equals("italic")) {
				style += SWT.ITALIC;
			}
		}
		return new Font(device, fontName, size, style);
	}
}
