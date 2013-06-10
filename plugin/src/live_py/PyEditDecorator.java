package live_py;

import java.util.ListResourceBundle;
import java.util.WeakHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Composite;
import org.python.pydev.editor.PyEdit;
import org.python.pydev.editor.codefolding.PySourceViewer;
import org.python.pydev.shared_core.callbacks.ICallbackListener;
import org.python.pydev.shared_ui.editor.BaseEditor;
import org.python.pydev.shared_ui.editor.IPyEditListener;
import org.python.pydev.shared_ui.editor.IPyEditListener4;

/**
 * This extension wraps the main PyEdit in an extra splitter and sets it up
 * like the compare editor. It then displays the live coding analysis whenever
 * the code changes.
 * @author Don Kirkby
 *
 */
public class PyEditDecorator implements IPyEditListener, IPyEditListener4 {
	private static WeakHashMap<BaseEditor, LiveCodingAnalyst> analystMap =
			new WeakHashMap<BaseEditor, LiveCodingAnalyst>();
	private static boolean isVisible;

	public static LiveCodingAnalyst getAnalyst(PyEdit editor)
	{
		// TODO: add locking
		return analystMap.get(editor);
	}
	
	public static void setAllVisibilities(boolean isVisible) {
		PyEditDecorator.isVisible = isVisible;
		
		for (LiveCodingAnalyst analyst : analystMap.values()) {
			analyst.setVisibility(isVisible);
		}
	}
	
	/**
	 * Wire up a new editor so that it will be displayed the way we want.
	 */
	@Override
	public void onEditorCreated(BaseEditor edit) {
		final LiveCodingAnalyst analyst = new LiveCodingAnalyst();
		analyst.setVisibility(isVisible);
		analystMap.put(edit, analyst);
		
		PyEdit pyEdit = (PyEdit) edit;
		
		pyEdit.onCreatePartControl.registerListener(
				new ICallbackListener<Composite>() {
			public Object call(Composite parent) {
				return analyst.createPartControl(parent);
			}
		});
		pyEdit.onAfterCreatePartControl.registerListener(
				new ICallbackListener<ISourceViewer>() {
			
			@Override
			public Object call(ISourceViewer newViewer) {
				return analyst.afterCreateControl(newViewer);
			}
		});
		pyEdit.onCreateSourceViewer.registerListener(
				new ICallbackListener<PySourceViewer>() {
			@Override
			public Object call(PySourceViewer newViewer) {
				return analyst.createSourceViewer(newViewer);
			}
		});
	}

	@Override
	public void onSave(BaseEditor edit, IProgressMonitor monitor) {
	}

	@Override
	public void onCreateActions(
			ListResourceBundle resources, 
			BaseEditor edit,
			IProgressMonitor monitor) {
	}

	@Override
	public void onDispose(BaseEditor edit, IProgressMonitor monitor) {
	}

	/**
	 * Wire up the main document and perform the first analysis.
	 */
	@Override
	public void onSetDocument(
			IDocument document, 
			BaseEditor edit,
			IProgressMonitor monitor) {
		LiveCodingAnalyst analyst = analystMap.get(edit);
		if (analyst != null) {
			analyst.onSetDocument(document, edit, monitor);
		}
	}
}
