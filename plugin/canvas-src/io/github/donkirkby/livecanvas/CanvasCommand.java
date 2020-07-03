package io.github.donkirkby.livecanvas;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CanvasCommand {
	public static final String BACKGROUND_COLOR = "bgcolor";
	public static final String CREATE_LINE = "create_line";
	public static final String CREATE_POLYGON = "create_polygon";
	public static final String CREATE_TEXT = "create_text";
	public static final String CREATE_IMAGE = "create_image";

	private String name;
	private ArrayList<Integer> coordinates =
			new ArrayList<>();
	private	Hashtable<String, String> options =
			new Hashtable<>();

	public static class FontOptions {
		private String name;
		private int size;
		private String[] styleNames;

		FontOptions(String name, int size, String[] styleNames) {
			this.name = name;
			this.size = size;
			this.styleNames = styleNames;
		}

		public String getName() {
			return name;
		}
		public int getSize() {
			return size;
		}
		public String[] getStyleNames() {
			return styleNames;
		}
	}

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
	int getCoordinateCount() {
		return coordinates.size();
	}
	public int getCoordinate(int index) {
		return coordinates.get(index);
	}
	public void setCoordinate(int index, int value) {
		coordinates.set(index, value);
	}

	public int[] getAllCoordinates() {
		int[] copy = new int[coordinates.size()];
		Iterator<Integer> itr = coordinates.iterator();
		for (int i = 0; i < copy.length; i++) {
			copy[i] = itr.next();
		}
		return copy;
	}
	public int[] getXCoordinates() {
		int[] copy = new int[coordinates.size()/2];
		Iterator<Integer> itr = coordinates.iterator();
		for (int i = 0; i < copy.length; i++) {
			copy[i] = itr.next();
			itr.next();
		}
		return copy;
	}
	public int[] getYCoordinates() {
		int[] copy = new int[coordinates.size()/2];
		Iterator<Integer> itr = coordinates.iterator();
		for (int i = 0; i < copy.length; i++) {
			itr.next();
			copy[i] = itr.next();
		}
		return copy;
	}
	@NotNull
	public FontOptions getFontOptions(String name) {
		String value = options.get(name);
		if (value == null) {
			value = "";
		}
		Pattern pattern = Pattern.compile("\\('([^']*)', (\\d+), '([^']*)'\\)");
		Matcher matcher = pattern.matcher(value);
		String fontName;
		int size;
		String[] styleNames;
		if ( ! matcher.matches()) {
			fontName = "Arial";
			size = 8;
			styleNames = new String[] {"normal"};
		} else {
			fontName = matcher.group(1);
			size = Integer.parseInt(matcher.group(2));
			styleNames = matcher.group(3).split(" ");
		}
		return new FontOptions(fontName, size, styleNames);
	}
}
