package live_py;

import java.util.ArrayList;
import java.util.Hashtable;

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
}
