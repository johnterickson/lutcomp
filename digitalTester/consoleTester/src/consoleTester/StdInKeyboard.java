package consoleTester;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import de.neemann.digital.gui.components.terminal.Keyboard.KeyboardInterface;

public class StdInKeyboard extends Thread implements KeyboardInterface {
	private String text = new String();
	private boolean hasReceived = false;
			
	@Override
    public synchronized int getChar() {
		if (text.length() == 0) {
			// System.out.println("Reading : {nothing}");
			return 0;
		} else {
			// System.out.println("Reading :" + text.charAt(0));
			return text.charAt(0);
		}
    }
	
	@Override
    public synchronized void deleteFirstChar() {
		if (text.length() > 0) {
			// System.out.println("Dequeing :" + text.charAt(0));
			text = text.substring(1);
		}
    }

	public synchronized void waitForFirstChar() throws InterruptedException {
		while (!hasReceived) {
			wait();
		}
	}

	private synchronized void putChars(String line) {
		text = text + line;
		hasReceived = true;
		notify();
	}

	@Override
	public void run() {
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		
		String line;
		while (true) {
			try {
				line = reader.readLine();
				if (line == null) {
					break;
				}
				line = line.trim() + '\n';
				putChars(line);
			} catch (IOException e) {
				break;
			}
		}
	}
}