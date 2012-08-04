package live_py;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.osgi.framework.Bundle;


public class LiveCodingResultsColumn extends LineNumberRulerColumn {
	private static final String COMMAND_PATTERN = "^\\s*#\\s*echo\\s+";
	private ITextViewer cachedTextViewer;
	private ArrayList<String> results;
	private final int MAX_WIDTH = 60;
	private int width;
	private int fixedWidth;
	private int scroll;
	private boolean isKeepAlive;
	private boolean isTurtle;
	private String scriptPath;
	private String[] environment;
	private PrintWriter processWriter;
	private BufferedReader processReader;

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
		if ( ! isActive(text))
		{
			results.clear();
			width = 0;
			return width;
		}
		try {
			width = 5;
			if (processWriter != null && ! isKeepAlive) {
				releaseProcess();
			}
			if (processWriter == null) {
				launchProcess();
			}
			try {
				if (isKeepAlive)
				{
					String encoded = encode(text);
					processWriter.println(encoded);
					processWriter.flush();
				}
				else
				{
					processWriter.write(text);
					processWriter.close();
				}
				
				results.clear();
				if (isKeepAlive)
				{
					String line = processReader.readLine();
					String resultText = 
							line == null
							? "No results returned."
							: decode(line);
					loadResults(
							new BufferedReader(new StringReader(resultText)));
				}
				else
				{
					loadResults(processReader);
				}
			} finally {
				if ( ! isKeepAlive)
				{
					releaseProcess();
				}
			}
		} catch (Exception e) {
			results.clear();
			String message = e.getMessage();
			if (message == null) {
				message = e.toString();
			}
			results.add(message);
		}
		width = fixedWidth > 0 ? fixedWidth : Math.min(width, MAX_WIDTH);
		for (int j = 0; j < results.size(); j++) {
			String line = results.get(j);
			line = line.substring(
					Math.min(scroll, line.length()),
					Math.min(scroll + width, line.length()));
			results.set(j, String.format("%1$-" + width + "s", line));
		}
		
		return width;
	}

	private void loadResults(BufferedReader reader) throws IOException {
		String line;
		do {
			line = reader.readLine();
			if (line != null) {
				results.add(line);
				width = Math.max(width, line.length() - scroll);
			}
		} while (line != null);
	}

	private void releaseProcess() throws IOException {
		processWriter.close();
		processReader.close();
		processWriter = null;
		processReader = null;
	}

	private void launchProcess() throws IOException {
		checkEnvironment();
		checkScriptPath();
		Runtime runtime = Runtime.getRuntime();
		ArrayList<String> arguments = new ArrayList<String>();
		arguments.add("python");
		arguments.add(scriptPath);
		if (isKeepAlive) {
			arguments.add("-k");
		}
		if (isTurtle) {
			arguments.add("-t");
		}
		Process process = runtime.exec(
				(String[])arguments.toArray(new String[arguments.size()]), 
				environment);
		processWriter = new PrintWriter(new BufferedWriter(
				new OutputStreamWriter(process.getOutputStream())));
		processReader = new BufferedReader(
				new InputStreamReader(process.getInputStream()));
	}
	
	private String encode(String source) {
		return source
				.replace("%", "%25")
				.replace("\r", "%0d")
				.replace("\n", "%0a");
	}
	
	private String decode(String source) {
		return source
				.replace("%0a", "\n")
				.replace("%0d", "\r")
				.replace("%25", "%");
	}

	private boolean isActive(String text) {
		if ( ! readFlag("on", text))
		{
			return false;
		}
		
		fixedWidth = readSetting("width", text);
		scroll = readSetting("scroll", text);
		isTurtle = readFlag("turtle", text);
		isKeepAlive = isTurtle || readFlag("keepalive", text);
		return true;
	}

	private boolean readFlag(String name, String text) {
		Pattern pattern = Pattern.compile(
				COMMAND_PATTERN + name + "\\s*$", 
				Pattern.MULTILINE);
		return pattern.matcher(text).find();
	}

	private int readSetting(String settingName, String text) {
		Pattern settingPattern = Pattern.compile(
				COMMAND_PATTERN + settingName + "\\s+(\\d+)\\s*$",
				Pattern.MULTILINE);
		Matcher matcher = settingPattern.matcher(text);
		int setting = 
				matcher.find() 
				? Integer.parseInt(matcher.group(1)) 
				: 0;
		return setting;
	}

	private void checkEnvironment() {
		if (environment != null)
		{
			return;
		}
			
		IWorkbench workbench = PlatformUI.getWorkbench();
		IWorkbenchWindow window = 
				workbench == null ? null : workbench.getActiveWorkbenchWindow();
		IWorkbenchPage activePage = 
				window == null ? null : window.getActivePage();
		
		IEditorPart editor = 
				activePage == null ? null : activePage.getActiveEditor();
		IEditorInput input = 
				editor == null ? null : editor.getEditorInput();
		IPath path = input instanceof FileEditorInput 
				? ((FileEditorInput)input).getPath()
				: null;
		if (path != null)
		{
			String pythonPath = String.format(
					"PYTHONPATH=%1$s", 
					path.removeLastSegments(1));
			environment = new String[] {pythonPath};
		}
	}
	
	private void checkScriptPath() {
		if (scriptPath != null)
		{
			return;
		}
		try {
			Bundle bundle = Activator.getDefault().getBundle();
			Path path = new Path("PySrc/code_tracer.py");
			URL bundleURL = FileLocator.find(bundle, path, null);
			URL fileURL;
			fileURL = FileLocator.toFileURL(bundleURL);
			scriptPath = fileURL.getPath();
			
			// Also get the path to report builder to unpack it from the bundle.
			Path path2 = new Path("PySrc/report_builder.py");
			URL bundle2URL = FileLocator.find(bundle, path2, null);
			FileLocator.toFileURL(bundle2URL);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
