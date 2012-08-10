package live_py;

/* Represents the text in an editor. Has methods for reading the currently
 * selected text and replacing it.
 */
public interface ValueSource {
	String getSelectedText();
	
	void replaceSelectedText(String text);
}
