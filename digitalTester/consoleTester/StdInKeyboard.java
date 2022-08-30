package consoleTester;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import de.neemann.digital.gui.components.terminal.Keyboard.KeyboardInterface;

public class StdInKeyboard implements KeyboardInterface {
	private final Object textLock = new Object();
	private String text;
	
	public StdInKeyboard() {
		new StdInReader(this);
	}
			
	@Override
    public int getChar() {
        synchronized (textLock) {
            if (text.length() == 0)
                return 0;
            else {
                return text.charAt(0);
            }
        }
    }
	
	@Override
    public void deleteFirstChar() {
        synchronized (textLock) {
            if (text.length() > 0) {
                text = text.substring(1);
            }
        }
    }
	
	private void pushChars(char[] cs) {
		synchronized (textLock) {
			text = text + (new String(cs));				
		}
	}
	
	private class StdInReader extends Thread {
		private final StdInKeyboard keyboard;
		
		public StdInReader(StdInKeyboard keyboard) {
			this.keyboard = keyboard;
		}
		
		@Override
		public void run() {
			BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
			
			String line;
			while (true) {
				try {
					line = reader.readLine() + '\n';
					this.keyboard.pushChars(line.toCharArray());
				} catch (IOException e) {
					break;
				}
			}
		}
	}
}