package live_py;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

public class StartMenuContribution extends ContributionItem {
    private static final String NO_DRIVER_NAME = "__live_coding__";
    
    private ILog log = Activator.getDefault().getLog();
    private String selectedName = NO_DRIVER_NAME;
    
    @Override
    public boolean isDynamic() {
        return true;
    }
    
    private void add(Menu menu, int index, ILaunchConfiguration launchConfig) {
        String text =
                launchConfig == null
                ? NO_DRIVER_NAME
                : launchConfig.getName();
        MenuItem menuItem = new MenuItem(menu, SWT.CHECK, index);
        menuItem.setText(text);
        menuItem.setSelection(selectedName.equals(text));
        menuItem.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                StartMenuContribution.this.selectedName = text;
                StartHandler handler = new StartHandler(launchConfig);
                try {
                    handler.execute();
                } catch (ExecutionException ex) {
                    log.log(new Status(
                            IStatus.ERROR,
                            Activator.PLUGIN_ID,
                            "Failed to start Live Coding.",
                            ex));
                }
            }
        });
    }
    
    @Override
    public void fill(Menu menu, int index) {
        add(menu, index++, null);
        String[] typeNames = {
                "org.python.pydev.debug.regularLaunchConfigurationType",
                // "org.python.pydev.debug.unittestLaunchConfigurationType"
        };
        ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
        for (String typeName : typeNames) {
            ILaunchConfigurationType launchConfigType =
                    launchManager.getLaunchConfigurationType(typeName);
            try {
                ILaunchConfiguration[] launchConfigurations = 
                        launchManager.getLaunchConfigurations(launchConfigType);
                for (ILaunchConfiguration launchConfig : launchConfigurations) {
                    if (launchConfig.getType().getIdentifier().startsWith(
                            "org.python.pydev")) {
                        add(menu, index++, launchConfig);
                    }
                }
            } catch (CoreException e) {
                log.log(new Status(
                        IStatus.ERROR, 
                        Activator.PLUGIN_ID, 
                        "Building menu failed.", 
                        e));
            }
        }

    }
}
