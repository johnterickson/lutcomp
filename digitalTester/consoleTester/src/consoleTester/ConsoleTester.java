package consoleTester;

import java.io.File;

import de.neemann.digital.gui.components.terminal.Keyboard;
import de.neemann.digital.testing.UnitTester;


public class ConsoleTester {
		
	public static void main(String[] args) throws Exception {
		var stdin = new StdInKeyboard();
		
		var tester = new UnitTester(new File(args[0]));
		var keyboardNode = (Keyboard)tester.getNode(n -> n instanceof Keyboard && ((Keyboard)n).getLabel().equals("TTYIN"));
		// System.out.println("Found keyboard: '" + keyboardNode.getLabel() + "'");
		keyboardNode.setKeyboard(stdin);
		
		stdin.start();
		stdin.waitForFirstChar();
		
		tester.runToBreak();
	}

}
