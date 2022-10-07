package com.johnterickson;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import de.neemann.digital.gui.components.terminal.Keyboard;
import de.neemann.digital.gui.components.terminal.Keyboard.KeyboardInterface;

public class FixedKeyboard implements KeyboardInterface {
	private String text = new String();
	private final Object textLock = new Object();
	private final Keyboard keyboard;
			
	public FixedKeyboard(Keyboard keyboard) {
		this.keyboard = keyboard;
	}

	@Override
    public int getChar() {
		synchronized (textLock) {
			if (text.length() == 0) {
				// System.err.println("Reading : {nothing}");
				return 0;
			} else {
				// System.err.println("Reading :" + text.charAt(0));
				return text.charAt(0);
			}
		}
    }
	
	@Override
    public void deleteFirstChar() {
		synchronized (textLock) {
			if (text.length() > 0) {
				// System.err.println("Dequeing :" + text.charAt(0));
				text = text.substring(1);
			}
		}
    }

	public void putChars(String line) {
		synchronized (textLock) {
			text = text + line;
		}
		this.keyboard.hasChanged();
	}
}