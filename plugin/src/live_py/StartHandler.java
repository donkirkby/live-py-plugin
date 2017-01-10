package live_py;

import live_py.LiveCodingAnalyst.Mode;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.python.pydev.editor.PyEdit;

/**
 * Show the live coding display.
 * @author don
 *
 */
public class StartHandler extends AbstractHandler {
    private ILaunchConfiguration launchConfig;
    
    public StartHandler() {
    }
    
    public StartHandler(ILaunchConfiguration launchConfig) {
        this.launchConfig = launchConfig;
    }
    
    public Object execute() throws ExecutionException {
        return execute(null);
    }
    
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        IEditorPart editor = window.getActivePage().getActiveEditor();
        if (editor instanceof PyEdit) {
            final LiveCodingAnalyst analyst =
                    PyEditDecorator.getAnalyst((PyEdit) editor);
            analyst.setMode(Mode.Display);
            analyst.setLaunchConfig(launchConfig);
        }
        return null;
    }
}
