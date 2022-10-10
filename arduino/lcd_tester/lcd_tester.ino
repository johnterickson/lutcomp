const int
 RS = 5,
 RW = 6,
 E = 7,
 D4 = 8,
 D5 = 9,
 D6 = 10,
 D7 = 11;

static byte ac = 0;

void dataIn() {
  for(int i = 0; i<4; i++) {
    pinMode(D4+i, INPUT);
  }

  digitalWrite(RW, HIGH);
}

void dataOut(byte d) {
  digitalWrite(RW, LOW);
  
  digitalWrite(D7, (d & 0x8) != 0 ? HIGH : LOW);
  digitalWrite(D6, (d & 0x4) != 0 ? HIGH : LOW);
  digitalWrite(D5, (d & 0x2) != 0 ? HIGH : LOW);
  digitalWrite(D4, (d & 0x1) != 0 ? HIGH : LOW);
  
  for(int i = 0; i<4; i++) {
    pinMode(D4+i, OUTPUT);
  }
}

void initPins() {
  digitalWrite(E, LOW);
  pinMode(E, OUTPUT);
  
  digitalWrite(RS, LOW);
  pinMode(RS, OUTPUT);
  
  digitalWrite(RW, HIGH);
  pinMode(RW, OUTPUT);
  
  dataIn();
}

void riseClock() {
  digitalWrite(E, LOW);
  delayMicroseconds(1);
  digitalWrite(E, HIGH);
  delayMicroseconds(1);
}

void fallClock() {
  digitalWrite(E, LOW);
  //delayMicroseconds(100);
}

void toggleE() {
  riseClock();
  fallClock();
}

void writeHalfByte(byte rs, byte d) {

  digitalWrite(RS, rs);

  dataOut(d);

  toggleE();

  dataIn();
}

void writeByte(byte rs, byte d) {

  digitalWrite(RS, rs);
  
  dataOut(d >> 4);

  toggleE();

  dataOut(d & 0xF);

  toggleE();

  dataIn();
  
}

void waitWhileBusy() {
  dataIn();
  digitalWrite(RS, LOW);

  while(1) {
    dataIn();
    byte busy = 0;
    //delayMicroseconds(1);
    riseClock();
    //delayMicroseconds(1);
    busy |= digitalRead(D7) != LOW ? 0x80 : 0x0;
    busy |= digitalRead(D6) != LOW ? 0x40 : 0x0;
    busy |= digitalRead(D5) != LOW ? 0x20 : 0x0;
    busy |= digitalRead(D4) != LOW ? 0x10 : 0x0;  
    fallClock();
    dataOut(0);
    //delayMicroseconds(1);
    dataIn();
    riseClock();
    //delayMicroseconds(1);
    busy |= digitalRead(D7) != LOW ? 0x8 : 0x0;
    busy |= digitalRead(D6) != LOW ? 0x4 : 0x0;
    busy |= digitalRead(D5) != LOW ? 0x2 : 0x0;
    busy |= digitalRead(D4) != LOW ? 0x1 : 0x0;
    fallClock();
    dataOut(0);
    Serial.println(busy, HEX);
    if ((busy & 0x80) == 0) {
      break;
    } else {
      //Serial.println(busy, HEX);
    }
    //Serial.print(busy, HEX);
    delayMicroseconds(1);
  }
}

void writeChar(char c) {
  if (c == '\n') {
    if (ac < 0x14) {
      ac = 0x40;
    } else if (ac < 0x40) {
      ac = 0x54;
    } else if (ac < 0x54) {
      ac = 0x14;
    } else {
      ac = 0;
    }
    waitWhileBusy();
    writeByte(0,0x80 | ac);
  } else {
    waitWhileBusy();
    writeByte(1,c);
    waitWhileBusy();
  
    ac += 1;
    
    if (ac == 0x00 + 20) {
      ac = 0x40;
      waitWhileBusy();
      writeByte(0,0x80 | ac);
    } else if (ac == 0x40 + 20) {
      ac = 0x14;
      waitWhileBusy();
      writeByte(0,0x80 | ac);
    } else if (ac == 0x14 + 20) {
      ac = 0x54;
      waitWhileBusy();
      writeByte(0,0x80 | ac);
    } else if (ac == 0x54 + 20) {
      ac = 0x00;
      waitWhileBusy();
      writeByte(0,0x80 | ac);
    }
  }
}

void writeStr(const char *c) {
  while (*c != '\0') {
    writeChar(*c);
    c++;
    delay(100);
  }
}

void clear() {
  waitWhileBusy();
  writeByte(0,0x1);
  waitWhileBusy();
  ac = 0;
}

void setup() {
  Serial.begin(1000000);
  
  Serial.println("init pins");
  initPins();
 
  delay(50);

  Serial.println("0x3X");
  writeHalfByte(0,3);
  delayMicroseconds(4500);

  Serial.println("0x3X");
  writeHalfByte(0,3);
  delayMicroseconds(4500);

  Serial.println("0x3X");
  writeHalfByte(0,3);
  delayMicroseconds(4500);

  Serial.println("0x2");
  writeHalfByte(0,2);
  
  Serial.println("0x28");
  writeByte(0,0x28);
  delay(1);

  Serial.println("0x0C");
  waitWhileBusy();
  writeByte(0,0x08 | 0x4);

  Serial.println("0x01");
  waitWhileBusy();
  clear();

  Serial.println("0x06");
  waitWhileBusy();
  writeByte(0,0x6);
}

static byte ch = ' ';

void loop() {
  writeChar(ch);
  if (ch == '~') {
    ch = '\n';
  } else if (ch == '\n') {
    ch = ' ';
  } else {
    ch += 1;
  }

  //writeStr("I am johnny and I likes potatoes. And I can beat you in R.P.S. I'll prove it.");
  delay(100);
  //clear();
}
