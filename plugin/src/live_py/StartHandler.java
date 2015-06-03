package live_py;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.python.pydev.editor.PyEdit;

/**
 * Show the live coding display.
 * @author don
 *
 */
public class StartHandler extends AbstractHandler {
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
        IEditorPart editor = window.getActivePage().getActiveEditor();
        if (editor instanceof PyEdit) {
            PyEditDecorator.getAnalyst((PyEdit) editor).setVisibility(true);
        }
        return null;
    }
}
