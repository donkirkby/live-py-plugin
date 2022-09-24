package io.github.donkirkby.livepy;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import io.github.donkirkby.livepy.LiveCodingAnalyst.Mode;

/**
 * Show the live coding display.
 * @author don
 *
 */
public class StartHandler extends AbstractHandler implements IElementUpdater {
	public static final String COMMAND_ID = "live-py.commands.start";
	
    private ILaunchConfiguration launchConfig;
    private ImageDescriptor inactiveIcon =
    		Activator.getImageDescriptor("icons/media-play-3x.png");
    private ImageDescriptor passIcon =
    		Activator.getImageDescriptor("icons/media-play-3x-pass.png");
    private ImageDescriptor failIcon =
    		Activator.getImageDescriptor("icons/media-play-3x-fail.png");
    
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
        final LiveCodingAnalyst analyst = LiveCodingAnalyst.getActiveAnalyst();
        if (analyst != null) {
        	analyst.setMode(Mode.Display);
        	analyst.setLaunchConfig(launchConfig);
		}
        Activator.getDefault().refreshElements(TurtleHandler.COMMAND_ID);
        return null;
    }

	@Override
	public void updateElement(
			UIElement element,
			@SuppressWarnings("rawtypes") Map parameters) {
		LiveCodingAnalyst analyst = LiveCodingAnalyst.getActiveAnalyst();
		ImageDescriptor chosenIcon = 
				analyst == null || 
					analyst.getMode() != LiveCodingAnalyst.Mode.Display
				? inactiveIcon
				: analyst.isPassing()
				? passIcon
				: failIcon;
		element.setIcon(chosenIcon);
	}
}
