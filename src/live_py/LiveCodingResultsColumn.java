package live_py;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.ArrayList;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.osgi.framework.Bundle;


public class LiveCodingResultsColumn extends LineNumberRulerColumn {
	private ITextViewer cachedTextViewer;
	private ArrayList<String> results;
	private final int MAX_WIDTH = 40;
	private int width;

	@Override
	public Control createControl(CompositeRuler parentRuler,
			Composite parentControl) {
		cachedTextViewer= parentRuler.getTextViewer();
		results = new ArrayList<String>();
		return super.createControl(parentRuler, parentControl);
	}
	
	@Override
	protected String createDisplayString(int line) {
		return line < results.size() ? results.get(line) : "";
	}
	
	@Override
	protected int computeNumberOfDigits() {
		String text = cachedTextViewer.getDocument().get();
		String[] arguments = new String[2];
		int i = 0;
		arguments[i++] = "python";
		arguments[i++] = findScript();
		Runtime runtime = Runtime.getRuntime();
		width = 5;
		try {
			Process process = runtime.exec(arguments);
			BufferedWriter writer = new BufferedWriter(
					new OutputStreamWriter(process.getOutputStream()));
			BufferedReader reader = new BufferedReader(
					new InputStreamReader(process.getInputStream()));
			
			try {
				writer.write(text);
				writer.close();
				
				results.clear();
				String line;
				do {
					line = reader.readLine();
					if (line != null) {
						results.add(line);
						width = Math.max(width, line.length());
					}
				} while (line != null);
			} finally {
				writer.close();
				reader.close();
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		width = Math.min(width, MAX_WIDTH);
		for (int j = 0; j < results.size(); j++) {
			String line = results.get(j);
			if (line.length() > MAX_WIDTH) {
				line = line.substring(0, MAX_WIDTH);
			}
			results.set(j, String.format("%1$-" + width + "s", line));
		}
		
		return width;
	}
	
	private String findScript()
	{
		try {
			Bundle bundle = Activator.getDefault().getBundle();
			Path path = new Path("PySrc/code_tracer.py");
			URL bundleURL = FileLocator.find(bundle, path, null);
			URL fileURL;
			fileURL = FileLocator.toFileURL(bundleURL);
			return fileURL.getPath();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
