package live_py;

import org.eclipse.core.expressions.EvaluationResult;
import org.eclipse.core.expressions.Expression;
import org.eclipse.core.expressions.ExpressionInfo;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISources;

/**
 * An expression that evaluates to true if and only if the current focus control
 * is the one provided. Has a very high priority so that our new handler will
 * replace the default one, but only when our control has the focus.
 */
public class ActiveFocusControlExpression extends Expression {

	private Control focusControl;

	/** Initialize an instance for a control. */
	public ActiveFocusControlExpression(Control control) {
		focusControl = control;
	}

	@Override
	public void collectExpressionInfo(ExpressionInfo info) {
		info.markDefaultVariableAccessed(); // give it a very high priority
		info.addVariableNameAccess(ISources.ACTIVE_SHELL_NAME);
		info.addVariableNameAccess(ISources.ACTIVE_WORKBENCH_WINDOW_NAME);
	}

	/** Check if our control has the focus. */
	@Override
	public EvaluationResult evaluate(IEvaluationContext context)
			throws CoreException {
		if (Display.getCurrent() != null && focusControl.isFocusControl()) {
			return EvaluationResult.TRUE;
		}
		return EvaluationResult.FALSE;
	}
}