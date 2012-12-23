package live_py;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.commands.ActionHandler;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.TextViewer;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerActivation;
import org.eclipse.ui.handlers.IHandlerService;

/**
 * This class adds new command handlers for the Copy command (Ctrl-C) when a
 * text viewer has the focus, and reverts to the default handlers when the text
 * viewer loses the focus. Based on a tutorial by David Green.
 * http://greensopinion.blogspot.ca/2009/10/key-bindings-in-eclipse-editors.html
 * 
 * @author Don Kirkby
 * 
 */
public class TextViewerSupport implements FocusListener, DisposeListener {

	// The text viewer that we watch for focus
	private final TextViewer textViewer;

	// The list of active handlers, or empty
	private List<IHandlerActivation> handlerActivations = new ArrayList<IHandlerActivation>();

	// Used to register the handlers
	private IHandlerService handlerService;

	/**
	 * Initialize a new instance, and add listeners to the text viewer to detect
	 * when it gains and loses focus.
	 * 
	 * @param textViewer
	 *            will receive the listeners.
	 */
	public TextViewerSupport(TextViewer textViewer) {
		this.textViewer = textViewer;
		StyledText textWidget = textViewer.getTextWidget();
		textWidget.addFocusListener(this);
		textWidget.addDisposeListener(this);

		IWorkbenchWindow window = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow();
		handlerService = (IHandlerService) window
				.getService(IHandlerService.class);

		if (textViewer.getTextWidget().isFocusControl()) {
			activateContext();
		}
	}

	/**
	 * Called when the text viewer loses focus, so we should remove the
	 * handlers.
	 */
	@Override
	public void focusLost(FocusEvent e) {
		deactivateContext();
	}

	/**
	 * Called when the text viewer gains focus, so we should add handlers.
	 */
	@Override
	public void focusGained(FocusEvent e) {
		activateContext();
	}

	/**
	 * Called when the text viewer is disposed, so we should remove the
	 * handlers.
	 */
	@Override
	public void widgetDisposed(DisposeEvent e) {
		deactivateContext();
	}

	/**
	 * Add handlers, if they haven't already been added.
	 */
	protected void activateContext() {
		if (handlerActivations.isEmpty()) {
			activateHandler(ITextOperationTarget.COPY,
					IWorkbenchCommandConstants.EDIT_COPY);
		}
	}

	/**
	 * Add a single handler.
	 * 
	 * @param operation
	 * @param actionDefinitionId
	 */
	protected void activateHandler(int operation, String actionDefinitionId) {
		StyledText textWidget = textViewer.getTextWidget();
		IHandler actionHandler = createActionHandler(operation,
				actionDefinitionId);
		IHandlerActivation handlerActivation = handlerService.activateHandler(
				actionDefinitionId, actionHandler,
				new ActiveFocusControlExpression(textWidget));

		handlerActivations.add(handlerActivation);
	}

	/**
	 * Create a handler that delegates to the text viewer.
	 * 
	 * @param operation
	 * @param actionDefinitionId
	 * @return
	 */
	private IHandler createActionHandler(final int operation,
			String actionDefinitionId) {
		Action action = new Action() {
			@Override
			public void run() {
				if (textViewer.canDoOperation(operation)) {
					textViewer.doOperation(operation);
				}
			}
		};
		action.setActionDefinitionId(actionDefinitionId);
		return new ActionHandler(action);
	}

	/**
	 * Remove all the handlers, if there are any.
	 */
	protected void deactivateContext() {
		if (!handlerActivations.isEmpty()) {
			for (IHandlerActivation activation : handlerActivations) {
				handlerService.deactivateHandler(activation);
				activation.getHandler().dispose();
			}
			handlerActivations.clear();
		}
	}
}
