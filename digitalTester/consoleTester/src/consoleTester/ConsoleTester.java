package consoleTester;

import java.io.File;

import de.neemann.digital.gui.components.terminal.Keyboard;
import de.neemann.digital.gui.components.terminal.Terminal;
import de.neemann.digital.testing.UnitTester;


public class ConsoleTester {
		
	public static void main(String[] args) throws Exception {
		
		
		var tester = new UnitTester(new File(args[0]));

		var ttyin = new StdInKeyboard();
		var keyboardNode = (Keyboard)tester.getNode(n -> n instanceof Keyboard && ((Keyboard)n).getLabel().equals("TTYIN"));
		keyboardNode.setKeyboard(ttyin);
		ttyin.start();
		ttyin.waitForFirstChar();

		String expected = null;
		if (args.length > 1) {
			expected = args[1];
		}

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
