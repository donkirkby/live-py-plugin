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
import java.util.ListResourceBundle;
import java.util.WeakHashMap;

import org.eclipse.compare.Splitter;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
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
import org.python.pydev.core.IPythonNature;
import org.python.pydev.core.MisconfigurationException;
import org.python.pydev.core.REF;
import org.python.pydev.core.Tuple;
import org.python.pydev.core.bundle.BundleUtils;
import org.python.pydev.core.callbacks.ICallbackListener;
import org.python.pydev.core.log.Log;
import org.python.pydev.editor.IPyEditListener;
import org.python.pydev.editor.IPyEditListener4;
import org.python.pydev.editor.PyEdit;
import org.python.pydev.editor.codefolding.PySourceViewer;
import org.python.pydev.runners.UniversalRunner;
import org.python.pydev.runners.UniversalRunner.AbstractRunner;

/**
 * This extension wraps the main PyEdit in an extra splitter and sets it up
 * like the compare editor. It then displays the live coding analysis whenever
 * the code changes.
 * @author Don Kirkby
 *
 */
public class LiveCodingAnalyst implements IPyEditListener, IPyEditListener4 {
	/**
	 * Making it true will print some debug info to stdout.
	 */
	private final static boolean DEBUG = true;
	private static WeakHashMap<PyEdit, LiveCodingAnalyst> analystMap =
			new WeakHashMap<PyEdit, LiveCodingAnalyst>();

	private ISourceViewer mainViewer;
	private IDocument mainDocument;
	private PyEdit pyEdit;
	private Document displayDocument;
	private SourceViewer displayViewer;
	private LiveCanvasView canvasView;
	private File scriptPath;
	private ArrayList<CanvasCommand> canvasCommands = 
			new ArrayList<CanvasCommand>();
	
	public static LiveCodingAnalyst getAnalyst(PyEdit editor)
	{
		// TODO: add locking
		return analystMap.get(editor);
	}
	
	/**
	 * Wire up a new editor so that it will be displayed the way we want.
	 */
	@Override
	public void onEditorCreated(PyEdit edit) {
		edit.onCreatePartControl.registerListener(
				new ICallbackListener<Composite>() {
			
			/**
			 * This callback inserts a new composite inside the standard window
			 * and then returns the left pane of the splitter as the new parent
			 * for the main editor controls.
			 * @param parent The standard window that usually holds the editor.
			 * @return The new control that the editor can be created in.
			 */
			public Object call(Composite parent) {
				Splitter splitter = new Splitter(parent, SWT.HORIZONTAL);
				
				Composite editorContent = new Composite(splitter, SWT.NONE);
				editorContent.setLayout(new FillLayout());
				GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
				editorContent.setLayoutData(gridData);
				
				Composite liveDisplay = new Composite(splitter, SWT.NONE);
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
			    splitter.setVisible(liveDisplay, true);

				return editorContent;
			}
		});
		edit.onAfterCreatePartControl.registerListener(
				new ICallbackListener<ISourceViewer>() {
			
			/**
			 * Copy the style settings from the main viewer to the display
			 * viewer.
			 * @param newViewer The main viewer that was just created.
			 * @return The main viewer.
			 */
			@Override
			public Object call(ISourceViewer newViewer) {
				if (newViewer instanceof PySourceViewer)
				{
					pyEdit = ((PySourceViewer)newViewer).getEdit();
					analystMap.put(pyEdit, LiveCodingAnalyst.this);
				}
				mainViewer = newViewer;
				displayViewer.getTextWidget().setFont(
						mainViewer.getTextWidget().getFont());
				return newViewer;
			}
		});
		edit.onCreateSourceViewer.registerListener(
				new ICallbackListener<PySourceViewer>() {

			/**
			 * Wire up the main viewer after it's created.
			 * @param viewer The main viewer that was just created.
			 * @return The main viewer.
			 */
			@Override
			public Object call(PySourceViewer newViewer) {

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
		});
	}

	@Override
	public void onSave(PyEdit edit, IProgressMonitor monitor) {
	}

	@Override
	public void onCreateActions(
			ListResourceBundle resources, 
			PyEdit edit,
			IProgressMonitor monitor) {
	}

	@Override
	public void onDispose(PyEdit edit, IProgressMonitor monitor) {
	}

	/**
	 * Wire up the main document and perform the first analysis.
	 */
	@Override
	public void onSetDocument(
			IDocument document, 
			PyEdit edit,
			IProgressMonitor monitor) {
		mainDocument = document;
		document.addDocumentListener(new IDocumentListener() {

			/**
			 * Analyse the document and display the results whenever the
			 * document changes.
			 */
			@Override
			public void documentChanged(DocumentEvent event) {
				analyseDocument(event.getDocument());
			}
			
			@Override
			public void documentAboutToBeChanged(DocumentEvent event) {
			}
		});
		
		// Perform the first analysis.
		refresh();
	}

	public void refresh() {
		// The analysis has to run on the display thread.
		Display.getDefault().asyncExec(new Runnable() {
		    public void run() {
		    	analyseDocument(mainDocument);
		    }
		});
	}

	/**
	 * Analyse the document and display the results.
	 * @param document
	 */
	private void analyseDocument(IDocument document) {
		try {
			Process process = launchProcess();
			PrintWriter writer = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(process.getOutputStream())));
			BufferedReader reader = new BufferedReader(
					new InputStreamReader(process.getInputStream()));
			BufferedReader errorReader = new BufferedReader(
					new InputStreamReader(process.getErrorStream()));
			try {
				writer.write(document.get());
				writer.close();
				if(DEBUG){
					System.out.println("Writing: " + document.get());
				}
				
				loadResults(reader);
				LiveCanvasView view = canvasView;
				if (view != null) {
					view.redraw();
				}
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
			displayDocument.set(message);
		}

		// Update the scroll position after changing the text.
		displayViewer.getTextWidget().setTopPixel(
				mainViewer.getTextWidget().getTopPixel());
	}

	private Process launchProcess() throws IOException {
		checkScriptPath();
		if(mainViewer == null){
			return null;
		}
		PyEdit pyEdit = ((PySourceViewer)mainViewer).getEdit();
		IPythonNature nature;
		try {
			nature = pyEdit.getPythonNature();
		} catch (MisconfigurationException e) {
			Log.log(e);
			return null;
		}
		AbstractRunner runner = UniversalRunner.getRunner(nature);
		ArrayList<String> argumentList = new ArrayList<String>();
		if (canvasView != null) {
			argumentList.add("-c");
			Rectangle bounds = canvasView.getBounds();
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
			if(DEBUG){
				System.out.println("load results line: "+line);
			}

			if (line != null) {
				if (isStarted) {
					printer.println(line);
				}
				else {
					if (line.equals("start_canvas")) {
						loadCanvasCommands(reader);
					}
					else {
						printer.println();
					}
					isStarted = true;
				}
			}
		} while (line != null);
		displayDocument.set(writer.toString());
	}
	
	private void loadCanvasCommands(BufferedReader reader) {
		canvasCommands.clear();
		CanvasReader canvasReader = new CanvasReader(reader);
		CanvasCommand command;
		boolean isDone;
		do {
			command = canvasReader.read();

			isDone = command == null || command.getName().equals("end_canvas");
			if ( ! isDone) {
				canvasCommands.add(command);
			}
		} while ( ! isDone);
	}

	private void checkScriptPath() {
		if (scriptPath != null)
		{
			return;
		}
		scriptPath = BundleUtils.getRelative(
				new Path("PySrc/code_tracer.py"),
				Activator.getDefault().getBundle());
		BundleUtils.getRelative(
				new Path("PySrc/report_builder.py"),
				Activator.getDefault().getBundle());
		BundleUtils.getRelative(
				new Path("PySrc/canvas.py"),
				Activator.getDefault().getBundle());
		BundleUtils.getRelative(
				new Path("PySrc/mock_turtle.py"),
				Activator.getDefault().getBundle());
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
}
