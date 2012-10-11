package live_py;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.eclipse.compare.Splitter;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IViewportListener;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.text.source.VerticalRuler;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.Bundle;
import org.python.pydev.core.IPythonNature;
import org.python.pydev.core.MisconfigurationException;
import org.python.pydev.core.REF;
import org.python.pydev.core.Tuple;
import org.python.pydev.core.bundle.BundleUtils;
import org.python.pydev.core.log.Log;
import org.python.pydev.editor.PyEdit;
import org.python.pydev.editor.codefolding.PySourceViewer;
import org.python.pydev.runners.UniversalRunner;
import org.python.pydev.runners.UniversalRunner.AbstractRunner;

/**
 * This actually runs the Python code and displays the analysis.
 * @author Don Kirkby
 *
 */
public class LiveCodingAnalyst {
	/**
	 * Making it true will print some debug info to stdout.
	 */
	private final static boolean DEBUG = false;
	
	private static LinkedBlockingQueue<AnalysisTask> toAnalyse =
			new LinkedBlockingQueue<AnalysisTask>();
	private static Thread analysisThread = new Thread() {
		public void run() {
			try {
				while (true) {
					AnalysisTask last = toAnalyse.take();
					AnalysisTask next;
					do {
						next = toAnalyse.poll(0, TimeUnit.SECONDS);
						if (next != null)
						{
							last = next;
						}
					} while (next != null);
					last.analyst.analyseDocument(last.sourceCode, last.bounds);
				}
			} catch (InterruptedException e) {
				// Just exit the thread if it was interrupted.
			}
		};
	};
	
	static {
		analysisThread.setDaemon(true);
		analysisThread.start();
	}
	
	private class AnalysisTask {
		public String sourceCode;
		public LiveCodingAnalyst analyst;
		public Rectangle bounds;
	}

	private ISourceViewer mainViewer;
	private IDocument mainDocument;
	private Document displayDocument;
	private SourceViewer displayViewer;
	private LiveCanvasView canvasView;
	private File scriptPath;
	private ArrayList<CanvasCommand> canvasCommands = 
			new ArrayList<CanvasCommand>();
	private Composite liveDisplay;
	private Splitter splitter;
	private boolean isVisible;

	/**
	 * This callback inserts a new composite inside the standard window
	 * and then returns the left pane of the splitter as the new parent
	 * for the main editor controls.
	 * @param parent The standard window that usually holds the editor.
	 * @return The new control that the editor can be created in.
	 */
	public Object createPartControl(Composite parent) {
		splitter = new Splitter(parent, SWT.HORIZONTAL);
		
		Composite editorContent = new Composite(splitter, SWT.NONE);
		editorContent.setLayout(new FillLayout());
		GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
		editorContent.setLayoutData(gridData);
		
		liveDisplay = new Composite(splitter, SWT.NONE);
		liveDisplay.setLayout(new FillLayout());
		GridData gridData2 = new GridData(SWT.FILL, SWT.FILL, true, true);
		liveDisplay.setLayoutData(gridData2);
		
		VerticalRuler ruler = new VerticalRuler(12);
		int styles = 
				SWT.V_SCROLL | 
				SWT.H_SCROLL | 
				SWT.MULTI | 
				SWT.BORDER | 
				SWT.FULL_SELECTION;
		displayViewer = 
				new SourceViewer(liveDisplay, ruler, styles);
		SourceViewerConfiguration config = 
				new SourceViewerConfiguration();
		displayViewer.configure(config);
		displayDocument = new Document("");
		displayViewer.setDocument(displayDocument);
		
		displayViewer.addViewportListener(new IViewportListener() {
			
			/**
			 * Update the scroll bar of the main viewer when the
			 * display viewer is scrolled.
			 */
			@Override
			public void viewportChanged(int verticalOffset) {
				if (mainViewer != null) {
					mainViewer.getTextWidget().setTopPixel(
							verticalOffset);
				}
			}
		});
		
	    splitter.setVisible(editorContent, true);
	    splitter.setVisible(liveDisplay, isVisible);

		return editorContent;
	}
	
	/**
	 * Copy the style settings from the main viewer to the display
	 * viewer.
	 * @param newViewer The main viewer that was just created.
	 * @return The main viewer.
	 */
	public Object afterCreateControl(ISourceViewer newViewer) {
		mainViewer = newViewer;
		displayViewer.getTextWidget().setFont(
				mainViewer.getTextWidget().getFont());
		return newViewer;
	}

	/**
	 * Wire up the main viewer after it's created.
	 * @param viewer The main viewer that was just created.
	 * @return The main viewer.
	 */
	public Object createSourceViewer(PySourceViewer newViewer) {
		newViewer.addViewportListener(new IViewportListener() {
			
			/**
			 * Update the scroll bar of the display viewer when the main
			 * viewer is scrolled.
			 * @param viewer The main viewer.
			 * @return
			 */
			@Override
			public void viewportChanged(int verticalOffset) {
				if (displayViewer != null) {
					displayViewer.getTextWidget().setTopPixel(
							verticalOffset);
				}
			}
		});
		return newViewer;
	}
	
	/**
	 * Set the visibility of the live coding display.
	 * @param isVisible
	 */
	public void setVisibility(final boolean isVisible) {
		this.isVisible = isVisible;
		// Can only change visibility on the UI thread.
		Display.getDefault().asyncExec(new Runnable() {
		    public void run() {
		    	if (splitter != null) {
		    		splitter.setVisible(
		    				liveDisplay, 
		    				isVisible);
		    	}
		    }
		});
	}

	/**
	 * Wire up the main document and perform the first analysis.
	 */
	public void onSetDocument(
			IDocument document, 
			PyEdit edit,
			IProgressMonitor monitor) {
		mainDocument = document;
		// TODO: Why does getAction always return null?
		IAction enableAction = edit.getAction("live-py.enable.action");
	    setVisibility(
	    		enableAction != null 
	    		? enableAction.isChecked()
	    		: isVisible);
		document.addDocumentListener(new IDocumentListener() {

			/**
			 * Analyse the document and display the results whenever the
			 * document changes.
			 */
			@Override
			public void documentChanged(DocumentEvent event) {
				addAnalysisTask(event.getDocument());
			}
			
			@Override
			public void documentAboutToBeChanged(DocumentEvent event) {
			}
		});
		
		// Perform the first analysis.
		refresh();
	}

	public void refresh() {
		addAnalysisTask(mainDocument);
	}

	/**
	 * Analyse the document and display the results.
	 * @param document
	 */
	private void analyseDocument(String sourceCode, Rectangle bounds) {
		try {
			Process process = launchProcess(bounds);
			if (process == null)
			{
				return;
			}
			PrintWriter writer = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(process.getOutputStream())));
			BufferedReader reader = new BufferedReader(
					new InputStreamReader(process.getInputStream()));
			BufferedReader errorReader = new BufferedReader(
					new InputStreamReader(process.getErrorStream()));
			try {
				writer.write(sourceCode);
				writer.close();
				if(DEBUG){
					System.out.println("Writing: " + sourceCode);
				}
				
				loadResults(reader);
				if (DEBUG) {
					String line;
					do
					{
						line = errorReader.readLine();
						if (line != null) {
							System.out.println(line);
						}
					} while (line != null);
				}
			} finally {
				writer.close();
				reader.close();
				errorReader.close();
			}
		} catch (Exception e) {
			String message = e.getMessage();
			if (message == null) {
				message = e.toString();
			}
			displayResults(message);
		}
	}

	private void displayResults(final String results) {
		// The document can only be updated from the display thread.
		Display.getDefault().asyncExec(new Runnable() {
		    public void run() {
				LiveCanvasView view = canvasView;
				if (view != null) {
					view.redraw();
				}
		    	displayDocument.set(results);

				// Update the scroll position after changing the text.
				displayViewer.getTextWidget().setTopPixel(
						mainViewer.getTextWidget().getTopPixel());
		    }
		});
	}

	private Process launchProcess(Rectangle bounds) throws IOException {
		checkScriptPath();
		if(mainViewer == null){
			return null;
		}
		PyEdit pyEdit = ((PySourceViewer)mainViewer).getEdit();
		IPythonNature nature;
		try {
			nature = pyEdit.getPythonNature();
			if (nature == null)
			{
				return null;
			}
		} catch (MisconfigurationException e) {
			Log.log(e);
			return null;
		}
		AbstractRunner runner = UniversalRunner.getRunner(nature);
		ArrayList<String> argumentList = new ArrayList<String>();
		if (bounds != null) {
			argumentList.add("-c");
			argumentList.add("-x");
			argumentList.add(Integer.toString(bounds.width));
			argumentList.add("-y");
			argumentList.add(Integer.toString(bounds.height));
		}
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

	private void loadResults(BufferedReader reader) throws IOException {
		String line;
		StringWriter writer = new StringWriter();
		PrintWriter printer = new PrintWriter(writer);
		boolean isStarted = false;
		do {
			line = reader.readLine();

			if (line != null) {
				if(DEBUG){
					System.out.println("load results line: "+line);
				}
				if (isStarted) {
					printer.println(line);
				}
				else {
					if (line.equals("start_canvas")) {
						loadCanvasCommands(reader);
					}
					else {
						canvasCommands = new ArrayList<CanvasCommand>();
						printer.println(line);
					}
					isStarted = true;
				}
			}
		} while (line != null);
		displayResults(writer.toString());
	}
	
	private void loadCanvasCommands(BufferedReader reader) {
		ArrayList<CanvasCommand> newCommands = new ArrayList<CanvasCommand>();
		CanvasReader canvasReader = new CanvasReader(reader);
		CanvasCommand command;
		boolean isDone;
		do {
			command = canvasReader.read();

			isDone = command == null || command.getName().equals("end_canvas");
			if ( ! isDone) {
				newCommands.add(command);
			}
		} while ( ! isDone);
		canvasCommands = newCommands;
	}

	private void checkScriptPath() {
		if (scriptPath != null)
		{
			return;
		}
		Bundle bundle = Activator.getDefault().getBundle();
		Path sourceFolder = new Path("PySrc");
		scriptPath = BundleUtils.getRelative(
				sourceFolder.append("code_tracer.py"),
				bundle);
		String[] otherScripts = new String[] {
				"report_builder.py",
				"canvas.py",
				"exec_python2.py",
				"exec_python3.py",
				"mock_turtle.py"
		};
		for (String script : otherScripts) {
			BundleUtils.getRelative(
					sourceFolder.append(script),
					bundle);
		}
		if(DEBUG){
			System.out.println("Script path: "+scriptPath);
		}
	}

	public LiveCanvasView getCanvasView() {
		return canvasView;
	}

	public void setCanvasView(LiveCanvasView canvasView) {
		this.canvasView = canvasView;
	}

	public ArrayList<CanvasCommand> getCanvasCommands() {
		return canvasCommands;
	}

	private void addAnalysisTask(IDocument document) {
		if (document == null) {
			return;
		}
		
		AnalysisTask task = new AnalysisTask();
		task.sourceCode = document.get();
		task.analyst = LiveCodingAnalyst.this;
		task.bounds = 
				canvasView != null
				? canvasView.getBounds()
				: null;
		toAnalyse.add(task);
	}
}
