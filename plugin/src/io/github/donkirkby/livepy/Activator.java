package io.github.donkirkby.livepy;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "live-py"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;
	
	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		IWorkbenchWindow workbenchWindow = getWorkbench().getActiveWorkbenchWindow();
		IPartService partService = workbenchWindow.getPartService();
	    if (partService != null) {
	    	partService.addPartListener(new IPartListener2() {
				@Override
				public void partVisible(IWorkbenchPartReference partRef) {
				}
				
				@Override
				public void partOpened(IWorkbenchPartReference partRef) {
				}
				
				@Override
				public void partInputChanged(IWorkbenchPartReference partRef) {
				}
				
				@Override
				public void partHidden(IWorkbenchPartReference partRef) {
				}
				
				@Override
				public void partDeactivated(IWorkbenchPartReference partRef) {
				}
				
				@Override
				public void partClosed(IWorkbenchPartReference partRef) {
				}
				
				@Override
				public void partBroughtToTop(IWorkbenchPartReference partRef) {
				}
				
				@Override
				public void partActivated(IWorkbenchPartReference partRef) {
					refreshButtons();
				}
			});
	    }
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Refresh any elements registered against the command with the given id.
	 */
	public void refreshElements(String commandId) {
		IWorkbenchWindow workbenchWindow = getWorkbench().getActiveWorkbenchWindow();
		ICommandService commandService = workbenchWindow.getService(ICommandService.class);
	    if (commandService != null) {
			commandService.refreshElements(commandId, null);
	    }
	}
	
	private void refreshButtons() {
		refreshElements(StartHandler.COMMAND_ID);
		refreshElements(TurtleHandler.COMMAND_ID);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
}
