package com.johnterickson;

import de.neemann.digital.gui.components.terminal.TerminalInterface;

public class InMemoryTerminal implements TerminalInterface {
	private final boolean echo;
	private String text = new String();

	public InMemoryTerminal(boolean echo) {
		this.echo = echo;
	}

	@Override
	public synchronized void addChar(char c) {
		text += c;
		if (echo) {
			System.out.print(c);
		}
	}

	@Override
	public synchronized String getText() {
		return text;
	}

}