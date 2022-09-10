package consoleTester;

import de.neemann.digital.gui.components.terminal.TerminalInterface;

public class InMemoryTerminal implements TerminalInterface {
	private final boolean echo;

	public InMemoryTerminal(boolean echo) {
		this.echo = echo;
	}

	private String text = new String();

	@Override
	public synchronized void addChar(char c) {
		text += c;
		if (echo) {
			System.out.print(c);
		}
	}

	@Override
	public String getText() {
		return text;
	}

}