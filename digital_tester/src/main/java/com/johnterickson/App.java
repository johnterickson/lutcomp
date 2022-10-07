package com.johnterickson;

import java.io.File;

import de.neemann.digital.gui.components.terminal.Keyboard;
import de.neemann.digital.gui.components.terminal.Terminal;
import de.neemann.digital.testing.UnitTester;

public class App {
		
	public static void main(String[] args) throws Exception {
		
		var tester = new UnitTester(new File(args[0]));

		String expected = null;
		String keyboardLabel = "TTYIN";
		String keyboardInput = null;
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
						keyboardLabel = value;
						break;
					case "input":
						keyboardInput = value;
						break;
					default:
						throw new RuntimeException("don't know " + name);
				}
			}
		}

		if (expected == null) {
			System.err.println("No expected output provided.");
			System.exit(1);
		}

		final String keyboard_name = keyboardLabel;

		var keyboardNode = (Keyboard)tester.getNode(n -> n instanceof Keyboard && ((Keyboard)n).getLabel().equals(keyboard_name));
		StdInKeyboard stdinKeyboard = null;
		FixedKeyboard fixedKeyboard = null;
		if (keyboardInput == null) {
			stdinKeyboard = new StdInKeyboard(keyboardNode);
			keyboardNode.setKeyboard(stdinKeyboard);
		} else {
			fixedKeyboard = new FixedKeyboard(keyboardNode);
			keyboardNode.setKeyboard(fixedKeyboard);
		}

		var terminal = new InMemoryTerminal(expected == null);
		var terminalNode = (Terminal)tester.getNode(n -> n instanceof Terminal && ((Terminal)n).getLabel().equals("TTYOUT"));
		terminalNode.setTerminalInterface(terminal);

		// run some cycles to stabilize things
		// System.err.println("Initialize.");
		for (int i = 0; i < 100; i++) {
			tester.getModel().doStep();
		}

		// System.err.println("Run.");
		if (stdinKeyboard != null) {
			stdinKeyboard.start();
			stdinKeyboard.join();
		}

		if (fixedKeyboard != null) {
			fixedKeyboard.putChars(keyboardInput);
		}

		tester.runToBreak();
		
		if (expected != null) {
			expected = expected.trim();
			String actual = terminal.getText().trim();
			if (!expected.equals(actual)) {
				System.err.println("Expected output: `" + expected + "`");
				System.err.println("Actual output: `" + actual + "`");
				System.exit(1);
			}
		}
	}
}
