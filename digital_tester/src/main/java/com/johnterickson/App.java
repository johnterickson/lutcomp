package com.johnterickson;

import java.io.File;

import de.neemann.digital.gui.components.terminal.Keyboard;
import de.neemann.digital.gui.components.terminal.Terminal;
import de.neemann.digital.testing.UnitTester;

public class App {
		
	public static void main(String[] args) throws Exception {
		
		var tester = new UnitTester(new File(args[0]));

		String expected = null;
		String keyboard = "TTYIN";
		for (int i = 1; i < args.length; i++) {
			String arg = args[i];
			int equals = arg.indexOf('=');
			if (equals > 0) {
				String name = arg.substring(0, equals);
				String value = arg.substring(equals + 1);
				switch (name) {
					case "expected":
						expected = value;
						break;
					case "keyboard":
						keyboard = value;
						break;
				}
			}
		}
		if (args.length > 1) {
			expected = args[1];
		}

		final String keyboard_name = keyboard;

		var ttyin = new StdInKeyboard();
		var keyboardNode = (Keyboard)tester.getNode(n -> n instanceof Keyboard && ((Keyboard)n).getLabel().equals(keyboard_name));
		keyboardNode.setKeyboard(ttyin);
		ttyin.start();
		ttyin.join();

		var ttyout = new InMemoryTerminal(expected == null);
		var terminalNode = (Terminal)tester.getNode(n -> n instanceof Terminal && ((Terminal)n).getLabel().equals("TTYOUT"));
		terminalNode.setTerminalInterface(ttyout);

		tester.runToBreak();
		
		if (expected != null) {
			String actual = ttyout.getText().trim();
			if (!expected.equals(actual)) {
				System.err.println("Expected output: `" + expected + "`");
				System.err.println("Actual output: `" + actual + "`");
				System.exit(1);
			}
		}
	}
}
