package consoleTester;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import de.neemann.digital.gui.components.terminal.Keyboard.KeyboardInterface;

public class StdInKeyboard extends Thread implements KeyboardInterface {
	private final Object textLock = new Object();
	private String text = new String();
	private boolean hasRead = false;
			
	@Override
    public int getChar() {
        synchronized (textLock) {
            if (text.length() == 0) {
            	//System.out.println("Reading : {nothing}");
                return 0;
            } else {
            	//System.out.println("Reading :" + text.charAt(0));
            	hasRead = true;
                return text.charAt(0);
            }
        }
    }
	
	@Override
    public void deleteFirstChar() {
        synchronized (textLock) {
            if (hasRead && text.length() > 0) {
            	//System.out.println("Dequeing :" + text.charAt(0));
                text = text.substring(1);
                hasRead = false;
            }
        }
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
				synchronized (textLock) {
					//System.out.println("Enqueing :" + line);
					text = text + line;				
				}
			} catch (IOException e) {
				break;
			}
		}
	}
}