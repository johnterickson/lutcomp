package consoleTester;

import java.io.File;
import java.io.IOException;

import de.neemann.digital.testing.UnitTester;
import de.neemann.digital.testing.UnitTester.TestException;
import de.neemann.digital.core.NodeException;
import de.neemann.digital.draw.elements.PinException;
import de.neemann.digital.draw.library.ElementNotFoundException;
import de.neemann.digital.gui.components.terminal.Keyboard;


public class ConsoleTester {
		
	public static void main(String[] args) throws TestException, IOException, ElementNotFoundException, PinException, NodeException {
		var stdin = new StdInKeyboard();
		
		var tester = new UnitTester(new File(args[0]));
		var keyboardNode = (Keyboard)tester.getNode(n -> n instanceof Keyboard && ((Keyboard)n).getLabel().equals("TTYIN"));
		// System.out.println("Found keyboard: '" + keyboardNode.getLabel() + "'");
		keyboardNode.setKeyboard(stdin);
		
		stdin.start();
		
		tester.runToBreak();
	}

}
