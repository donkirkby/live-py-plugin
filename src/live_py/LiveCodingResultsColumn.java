package live_py;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


public class LiveCodingResultsColumn extends LineNumberRulerColumn {
	private ITextViewer cachedTextViewer;
	private String startText;

	@Override
	public Control createControl(CompositeRuler parentRuler,
			Composite parentControl) {
		cachedTextViewer= parentRuler.getTextViewer();
		return super.createControl(parentRuler, parentControl);
	}
	
	@Override
	protected String createDisplayString(int line) {
		return super.createDisplayString(line) + startText;
	}
	
	@Override
	protected int computeNumberOfDigits() {
		String text = cachedTextViewer.getDocument().get();
		startText = text.substring(0, 10);
		return super.computeNumberOfDigits() + 10;
	}
}
