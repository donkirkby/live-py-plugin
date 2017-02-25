package live_py;

import live_py.LiveCodingAnalyst.Mode;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.python.pydev.editor.PyEdit;

/**
 * Show the live coding display.
 * @author don
 *
 */
public class StartHandler extends AbstractHandler implements IElementUpdater {
    private ILaunchConfiguration launchConfig;
    private ImageDescriptor inactiveIcon = Activator.getImageDescriptor("icons/media-play-3x.png");
    private ImageDescriptor passIcon = Activator.getImageDescriptor("icons/media-play-3x-pass.png");
    private ImageDescriptor failIcon = Activator.getImageDescriptor("icons/media-play-3x-fail.png");
    
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
        final LiveCodingAnalyst analyst = getAnalyst();
        if (analyst != null) {
        	analyst.setMode(Mode.Display);
        	analyst.setLaunchConfig(launchConfig);
		}
        return null;
    }

	private LiveCodingAnalyst getAnalyst() {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        IEditorPart editor = window.getActivePage().getActiveEditor();
        LiveCodingAnalyst analyst = null;
        if (editor instanceof PyEdit) {
			analyst = PyEditDecorator.getAnalyst((PyEdit) editor);
        }
		return analyst;
	}

	@Override
	public void updateElement(UIElement element, @SuppressWarnings("rawtypes") Map parameters) {
		LiveCodingAnalyst analyst = getAnalyst();
		ImageDescriptor chosenIcon = 
				analyst.getMode() == LiveCodingAnalyst.Mode.Hidden
				? inactiveIcon
				: analyst.isPassing()
				? passIcon
				: failIcon;
		element.setIcon(chosenIcon);
	}
}
