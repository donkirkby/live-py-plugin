package io.github.donkirkby.livecanvas;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CanvasCommand {
	private String name;
	private ArrayList<Integer> coordinates = 
			new ArrayList<Integer>();
	private	Hashtable<String, String> options = 
			new Hashtable<String, String>();
	
	public static class FontOptions {
		private String name;
		private int size;
		private String[] styleNames;
		
		public FontOptions(String name, int size, String[] styleNames) {
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
	public int getCoordinateCount() {
		return coordinates.size();
	}
	public int getCoordinate(int index) {
		return coordinates.get(index);
	}
	public int[] getAllCoordinates() {
		int[] copy = new int[coordinates.size()];
		Iterator<Integer> itr = coordinates.iterator();
		for (int i = 0; i < copy.length; i++) {
			copy[i] = itr.next().intValue();
		}
		return copy;
	}
	public FontOptions getFontOptions(String name) {
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
		return new FontOptions(fontName, size, styleNames);
	}
}
