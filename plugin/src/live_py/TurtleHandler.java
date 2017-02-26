package live_py;

import live_py.LiveCodingAnalyst.Mode;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;
import org.python.pydev.editor.PyEdit;

/**
 * Show the live coding display.
 * @author don
 *
 */
public class TurtleHandler extends AbstractHandler implements IElementUpdater {
	public static final String COMMAND_ID = "live-py.commands.turtle";

	private ImageDescriptor inactiveIcon =
			Activator.getImageDescriptor("icons/turtle.png");
    private ImageDescriptor passIcon =
    		Activator.getImageDescriptor("icons/turtle-pass.png");
    private ImageDescriptor failIcon =
    		Activator.getImageDescriptor("icons/turtle-fail.png");

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
        IEditorPart editor = window.getActivePage().getActiveEditor();
        if (editor instanceof PyEdit) {
            PyEditDecorator.getAnalyst((PyEdit) editor).setMode(Mode.Turtle);
        }
        Activator.getDefault().refreshElements(StartHandler.COMMAND_ID);
        return null;
    }

	@Override
	public void updateElement(
			UIElement element,
			@SuppressWarnings("rawtypes") Map parameters) {
		LiveCodingAnalyst analyst = LiveCodingAnalyst.getActiveAnalyst();
		ImageDescriptor chosenIcon = 
				analyst.getMode() != LiveCodingAnalyst.Mode.Turtle
				? inactiveIcon
				: analyst.isPassing()
				? passIcon
				: failIcon;
		element.setIcon(chosenIcon);
	}
}
