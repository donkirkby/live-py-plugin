package live_py;

import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IEditorPart;
import org.python.pydev.editor.PyEdit;

/**
 * Stop the live coding display.
 * @author don
 *
 */
public class StopAction extends LiveCodingAction {
	@Override
	public void run(IAction action) {
        IEditorPart editor = getActiveEditor();
        if (editor instanceof PyEdit) {
            PyEditDecorator.getAnalyst((PyEdit) editor).setVisibility(false);
        }
	}
}
