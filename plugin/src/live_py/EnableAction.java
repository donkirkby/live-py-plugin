package live_py;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * Enable or disable the live coding display.
 * @author don
 *
 */
public class EnableAction implements IWorkbenchWindowActionDelegate {
	@Override
	public void run(IAction action) {
		PyEditDecorator.setAllVisibilities(action.isChecked());
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public void init(IWorkbenchWindow window) {
	}
}
