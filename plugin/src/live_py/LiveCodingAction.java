package live_py;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public abstract class LiveCodingAction implements IWorkbenchWindowActionDelegate {

    private IWorkbenchWindow window;

    @Override
    public abstract void run(IAction action);

    protected IEditorPart getActiveEditor() {
        IEditorPart editor = window.getActivePage().getActiveEditor();
        return editor;
    }

    @Override
    public void selectionChanged(IAction action, ISelection selection) {
    }

    @Override
    public void dispose() {
    }

    @Override
    public void init(IWorkbenchWindow window) {
        this.window = window;
    }

}