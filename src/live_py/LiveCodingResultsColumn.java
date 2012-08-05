package live_py;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.python.pydev.core.MisconfigurationException;
import org.python.pydev.core.REF;
import org.python.pydev.core.Tuple;
import org.python.pydev.core.bundle.BundleUtils;
import org.python.pydev.core.log.Log;
import org.python.pydev.editor.PyEdit;
import org.python.pydev.editor.codefolding.PySourceViewer;
import org.python.pydev.plugin.nature.PythonNature;
import org.python.pydev.runners.UniversalRunner;
import org.python.pydev.runners.UniversalRunner.AbstractRunner;


public class LiveCodingResultsColumn extends LineNumberRulerColumn {
	/**
	 * Making it true will print some debug info to stdout.
	 */
	private final static boolean DEBUG = false;
	
	private static final String COMMAND_PATTERN = "^\\s*#\\s*echo\\s+";
	private PyEdit pyEdit;
	private ArrayList<String> results;
	private final int MAX_WIDTH = 60;
	private int width;
	private int fixedWidth;
	private int scroll;
	private File scriptPath;

	@Override
	public Control createControl(CompositeRuler parentRuler,
			Composite parentControl) {
		ITextViewer textViewer = parentRuler.getTextViewer();
		if(textViewer instanceof PySourceViewer){
			pyEdit= ((PySourceViewer) textViewer).getEdit();
		}else if(DEBUG){
			System.out.println("Editor found is not PyEdit: "+textViewer);
			
		}
		results = new ArrayList<String>();
		return super.createControl(parentRuler, parentControl);
	}
	
	@Override
	protected String createDisplayString(int line) {
		return line < results.size() ? results.get(line) : "";
	}
	
	@Override
	protected int computeNumberOfDigits() {
		if(pyEdit == null){
			return 0;
		}
		String text = pyEdit.getDocument().get();
		if ( ! isActive(text))
		{
			results.clear();
			width = 0;
			return width;
		}
		width = 5;
		try {
			Process process = launchProcess();
			PrintWriter writer = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(process.getOutputStream())));
			BufferedReader reader = new BufferedReader(
					new InputStreamReader(process.getInputStream()));
			try {
				writer.write(text);
				writer.close();
				if(DEBUG){
					System.out.println("Writing: "+text);
				}
				
				results.clear();
				loadResults(reader);
			} finally {
				writer.close();
				reader.close();
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
			if(DEBUG){
				System.out.println("load results line: "+line);
			}

			if (line != null) {
				results.add(line);
				width = Math.max(width, line.length() - scroll);
			}
		} while (line != null);
	}

	private Process launchProcess() throws IOException {
		checkScriptPath();
		if(pyEdit == null){
			return null;
		}
		PythonNature nature;
		try {
			nature = (PythonNature) pyEdit.getPythonNature();
		} catch (MisconfigurationException e) {
			Log.log(e);
			return null;
		}
		AbstractRunner runner = UniversalRunner.getRunner(nature);
		ArrayList<String> argumentList = new ArrayList<String>();
		String[] arguments = 
				(String[])argumentList.toArray(new String[argumentList.size()]);
		File editorFile = pyEdit.getEditorFile();
		Tuple<Process, String> tuple = runner.createProcess(
				REF.getFileAbsolutePath(scriptPath), 
				arguments, 
				editorFile.getParentFile(), 
				null);
		if(tuple.o1 != null){
			if(DEBUG){
				System.out.println("Launched: "+tuple.o2);
			}
			Process process = tuple.o1;
			return process;
		}
		if(DEBUG){
			System.out.println("Unable to make launch.");
		}
		return null;
	}

	private boolean isActive(String text) {
		if ( ! readFlag("on", text))
		{
			return false;
		}
		
		fixedWidth = readSetting("width", text);
		scroll = readSetting("scroll", text);
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

	
	private void checkScriptPath() {
		if (scriptPath != null)
		{
			return;
		}
		scriptPath = BundleUtils.getRelative(
				new Path("PySrc/code_tracer.py"),
				Activator.getDefault().getBundle());
		if(DEBUG){
			System.out.println("Script path: "+scriptPath);
		}
	}
}
