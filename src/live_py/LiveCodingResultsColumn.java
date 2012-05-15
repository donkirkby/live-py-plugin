package live_py;

import org.eclipse.jface.text.source.LineNumberRulerColumn;


public class LiveCodingResultsColumn extends LineNumberRulerColumn {
	@Override
	protected String createDisplayString(int line) {
		return super.createDisplayString(line) + "x";
	}
	
	@Override
	protected int computeNumberOfDigits() {
		return super.computeNumberOfDigits() + 1;
	}
}
