const int
 DATA0 = 4,
 DATA1 = 5,
 DATA2 = 6,
 DATA3 = 7,
 DATA7 = 8,
 OE_   = 9,
 CP    = 10;

const byte ROWS = 4;
const byte COLS = 20;
const byte ROW_START[ROWS] = { 0x0, 0x40, 0x14, 0x54 };

//static byte ac = 0;
static byte current = 0;
static byte screen[ROWS*COLS] = {0};

byte logicalToPhysical(byte i) {
  byte row = i / COLS;
  byte col = i % COLS;
  return ROW_START[row] + col;
}

void dataIn() {
  for(int i = 0; i<4; i++) {
    pinMode(DATA0+i, INPUT);
  }

  digitalWrite(OE_, LOW);
}

void dataOut(byte d) {
  digitalWrite(OE_, HIGH);

  digitalWrite(DATA7, (d & 0x80) != 0 ? HIGH : LOW);
  digitalWrite(DATA3, (d & 0x08) != 0 ? HIGH : LOW);
  digitalWrite(DATA2, (d & 0x04) != 0 ? HIGH : LOW);
  digitalWrite(DATA1, (d & 0x02) != 0 ? HIGH : LOW);
  digitalWrite(DATA0, (d & 0x01) != 0 ? HIGH : LOW);
  
  for(int i = 0; i<4; i++) {
    pinMode(DATA0+i, OUTPUT);
  }
}

void initPins() {
  digitalWrite(CP, LOW);
  pinMode(CP, OUTPUT);
  
  digitalWrite(OE_, HIGH);
  pinMode(OE_, OUTPUT);

  digitalWrite(DATA7, LOW);
  pinMode(DATA7, OUTPUT);
 
  dataOut(0xFF);
}

void riseClock() {
  digitalWrite(CP, HIGH);
  //delayMicroseconds(1);
  digitalWrite(CP, LOW);
  //delayMicroseconds(1);
}

void writeHalfByte(byte rs, byte d) {
  rs = rs != 0 ? 0x80 : 0x00;
  dataOut(rs | d);
  riseClock();
}

void writeByte(byte rs, byte d) {
  rs = rs != 0 ? 0x80 : 0x00;
  
  dataOut(rs | ((d >> 4) & 0xF));
  riseClock();

  dataOut(rs | (d & 0xF));
  riseClock();
}

void waitWhileBusy() {
  // ST7066U pg 27 notes that there must be 
  // a 80+ us delay before checking the busy flag
  delayMicroseconds(100);
  while(1) {
    byte busy = 0;
    dataIn();    
    busy |= digitalRead(DATA3) != LOW ? 0x80 : 0x0;
    busy |= digitalRead(DATA2) != LOW ? 0x40 : 0x0;
    busy |= digitalRead(DATA1) != LOW ? 0x20 : 0x0;
    busy |= digitalRead(DATA0) != LOW ? 0x10 : 0x0;  
    dataOut(0);
    dataIn();
    busy |= digitalRead(DATA3) != LOW ? 0x8 : 0x0;
    busy |= digitalRead(DATA2) != LOW ? 0x4 : 0x0;
    busy |= digitalRead(DATA1) != LOW ? 0x2 : 0x0;
    busy |= digitalRead(DATA0) != LOW ? 0x1 : 0x0;
    dataOut(0);
    
    if ((busy & 0x80) == 0) {
      break;
    }
    
    delayMicroseconds(10);
  }
}

void scrollUp() {
  current -= COLS;

  byte i;
  for (i=0; i<(ROWS-1)*COLS; i++) {
    screen[i] = screen[i+COLS];
    waitWhileBusy();
    writeByte(0, 0x80 | logicalToPhysical(i));
    waitWhileBusy();
    writeByte(1, screen[i]);
  }
  for (; i<ROWS*COLS; i++) {
    waitWhileBusy();
    writeByte(0, 0x80 | logicalToPhysical(i));
    waitWhileBusy();
    writeByte(1, ' ');
  }
}

void writeChar(char c) {
  if (c == '\n') {
    byte next = ((current/COLS) + 1)*COLS;
    while (current < next) {
      screen[current] = ' ';
      current += 1;
    }
  } else {
    screen[current] = c;
    waitWhileBusy();
    writeByte(0, 0x80 | logicalToPhysical(current));
    waitWhileBusy();
    writeByte(1,c);
    current += 1;
  }
  
  if (current >= ROWS*COLS) {
    scrollUp();
  }
}

void writeStr(const char *c) {
  while (*c != '\0') {
    writeChar(*c);
    c++;
    delay(50);
  }
}

void clear() {
  waitWhileBusy();
  writeByte(0,0x1);
  current = 0;
  for(byte i=0; i<ROWS*COLS; i++) screen[i] = 0;
}

static byte ch = ' ';

void writeNext() {
  writeChar(ch);
  if (ch == 0x7F) {
    ch = '\n';
  } else if (ch == '\n') {
    ch = ' ';
  } else {
    ch += 1;
  }
}

void setup() {
//  Serial.begin(1000000);
  
//  Serial.println("init pins");
  initPins();
 
  delay(50);

//  Serial.println("0x3X");
  writeHalfByte(0,3);
  delayMicroseconds(4500);

//  Serial.println("0x3X");
  writeHalfByte(0,3);
  delayMicroseconds(4500);

//  Serial.println("0x3X");
  writeHalfByte(0,3);
  delayMicroseconds(4500);

//  Serial.println("0x2");
  writeHalfByte(0,2);
  delay(1);
  
//  Serial.println("0x28");
  writeByte(0,0x28);
  delay(1);

//  Serial.println("0x0C");
  waitWhileBusy();
  writeByte(0,0x08 | 0x4);

//  Serial.println("0x01");
  waitWhileBusy();
  clear();

//  Serial.println("0x06");
  waitWhileBusy();
  writeByte(0,0x6);

  writeNext();
}



void loop() {
  //writeNext();
  writeStr("I am johnny and I likes potatoes. And I can beat you in R.P.S. I'll prove it.\n");
  writeStr("\n");
  writeStr(" - Benny Erickson");
  delay(100);
  clear();
}
