package live_py;

import java.io.BufferedReader;
import java.io.IOException;

public class CanvasReader {
	private BufferedReader inputReader;
	private String nextLine;

	public CanvasReader(BufferedReader inputReader) {
		this.inputReader = inputReader;
	}

	public CanvasCommand read() {
		try {
			String line = nextLine != null ? nextLine : inputReader.readLine();
			if (line == null) {
				return null;
			}
			nextLine = null;
			CanvasCommand command = new CanvasCommand();
			command.setName(line);
			while (true) {
				nextLine = inputReader.readLine();
				if (nextLine == null || !nextLine.startsWith("    ")) {
					return command;
				}
				nextLine = nextLine.substring(4); // trim spaces
				int position = nextLine.indexOf('=');
				if (position == -1) {
					command.addCoordinate(Integer.parseInt(nextLine));
				} else {
					String name = nextLine.substring(0, position);
					String value = nextLine.substring(position+1);
					if (value.startsWith("'") && value.endsWith("'")) {
						value = value.substring(1, value.length() - 1);
					}
					command.setOption(name, value);
				}
			}
		} catch (IOException ex) {
			throw new RuntimeException(ex);
		}
	}

	public void close() {
		try {
			inputReader.close();
		} catch (IOException ex) {
			throw new RuntimeException(ex);
		}
	}
}
