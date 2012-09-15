package live_py;

import java.util.ListResourceBundle;
import java.util.WeakHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Composite;
import org.python.pydev.core.callbacks.ICallbackListener;
import org.python.pydev.editor.IPyEditListener;
import org.python.pydev.editor.IPyEditListener4;
import org.python.pydev.editor.PyEdit;
import org.python.pydev.editor.codefolding.PySourceViewer;

/**
 * This extension wraps the main PyEdit in an extra splitter and sets it up
 * like the compare editor. It then displays the live coding analysis whenever
 * the code changes.
 * @author Don Kirkby
 *
 */
public class PyEditDecorator implements IPyEditListener, IPyEditListener4 {
	private static WeakHashMap<PyEdit, LiveCodingAnalyst> analystMap =
			new WeakHashMap<PyEdit, LiveCodingAnalyst>();

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
		final LiveCodingAnalyst analyst = new LiveCodingAnalyst();
		analystMap.put(edit, analyst);
				
		edit.onCreatePartControl.registerListener(
				new ICallbackListener<Composite>() {
			public Object call(Composite parent) {
				return analyst.createPartControl(parent);
			}
		});
		edit.onAfterCreatePartControl.registerListener(
				new ICallbackListener<ISourceViewer>() {
			
			@Override
			public Object call(ISourceViewer newViewer) {
				return analyst.afterCreateControl(newViewer);
			}
		});
		edit.onCreateSourceViewer.registerListener(
				new ICallbackListener<PySourceViewer>() {
			@Override
			public Object call(PySourceViewer newViewer) {
				return analyst.createSourceViewer(newViewer);
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
		LiveCodingAnalyst analyst = analystMap.get(edit);
		if (analyst != null) {
			analyst.onSetDocument(document, edit, monitor);
		}
	}
}
