
#include <avr/interrupt.h>

const int EMPTY = -256;

const int
  RESET_ = 2,
  CLK = 3,
  HALT = 4,
  TTYOUT_CP = 5, //PD5, PCINT21
  TTYIN_CP = 6, //PD6, PCINT22
  TTYIN_OE_ = 7, //PD7, PCINT23
  LED = 13;

const int DATA_PINS[] = {
  8,9,10,11,14,15,16,17
};

const int QUEUE_LENGTH = 64;
int inputQueue[QUEUE_LENGTH] = {0};
int outputQueue[QUEUE_LENGTH] = {0};

unsigned long tickCount = 0;
unsigned long start_millis = 0;
//unsigned long breakPoint = 0;//1700/4*4;
//long dataBreakPoint = 0x23;
bool paused = false;
bool halted = false;
unsigned int delay_us = 0;

void initOutput(int pin, int initValue) {
  if (initValue != 0) {
    pinMode(pin, INPUT_PULLUP);
  } else {
    pinMode(pin, INPUT);
  }

  pinMode(pin, OUTPUT);
}

void setDataPinsInput() {
  for(int i=0; i<8;i++) {
    pinMode(DATA_PINS[i], INPUT);
  }
}

void setDataPinsOutput() {
  for(int i=0; i<8;i++) {
    byte b = outputQueue[0] == EMPTY
      ? 0
      : ((outputQueue[0] & 0xFF) | 0x80);
    initOutput(DATA_PINS[i], (b >> i) & 0x1);
  }
}

void init_queue(int queue[QUEUE_LENGTH]) {
  for(int i=0; i < QUEUE_LENGTH; i++) queue[i] = EMPTY;
}

void enqueue(int queue[QUEUE_LENGTH], byte b) {
  for(int i=0; i < QUEUE_LENGTH; i++) {
    if (queue[i] == EMPTY) {
      queue[i] = b;
      break;
    }
  }
}

int dequeue(int queue[QUEUE_LENGTH]) {
  int poppedValue = queue[0];
  for(int i=0; i<QUEUE_LENGTH-1; i++) {
    queue[i] = queue[i+1];
  }
  queue[QUEUE_LENGTH-1] = EMPTY;
  return poppedValue;
}

byte readDataPins() {
  byte b = 0;
  for(int i=0; i<8;i++) {
    b |= digitalRead(DATA_PINS[i]) << i;
  }
  return b;
}

ISR(PCINT2_vect){   // Port D, PCINT16 - PCINT23
  static bool prevTTYIN_OE_ = false;
  bool newTTYIN_OE_ = digitalRead(TTYIN_OE_);
  if (newTTYIN_OE_ == HIGH) {
    setDataPinsInput();
  } else {
    setDataPinsOutput();
  }
  if (!prevTTYIN_OE_ && newTTYIN_OE_) {
    //Serial.println("TTYIN_OE_ LOW->HIGH");
  } else if (prevTTYIN_OE_ && !newTTYIN_OE_) {
    //Serial.print("TTYIN_OE_ HIGH->LOW: 0x");
    //paused = true;
    //byte b = readDataPins();
    //Serial.print(b, HEX);
    //Serial.println();
  }
  prevTTYIN_OE_ = newTTYIN_OE_;

  static bool prevTTYOUT_CP = false;
  bool newTTYOUT_CP = digitalRead(TTYOUT_CP);
  if (!prevTTYOUT_CP && newTTYOUT_CP) {
    byte b = readDataPins();
    enqueue(inputQueue, b);
  }
  prevTTYOUT_CP = newTTYOUT_CP;

  static bool prevTTYIN_CP = false;
  bool newTTYIN_CP = digitalRead(TTYIN_CP);
  if (!prevTTYIN_CP && newTTYIN_CP) {
    byte dequeued = dequeue(outputQueue) & 0x7f;
    byte acked = readDataPins() & 0x7f;
    if (dequeued != acked) {
      Serial.print("TTL->ARDUINO ACK: 0x");
      Serial.print(dequeued, HEX);
      Serial.print(" vs 0x");
      Serial.println(acked, HEX);
    }
  }
  prevTTYIN_CP = newTTYIN_CP;
}


void tick(bool force) { 
  if (force || !paused) {
    digitalWrite(CLK, 1 ^ digitalRead(CLK));
    tickCount += 1;

/*
    if ((tickCount % 8) == 7){
      char str[20];
      byte b = readDataPins();
      sprintf(str,"data_bus: %02x", b);
      Serial.println(str);
      Serial.flush();
      if (b == dataBreakPoint){
        Serial.print("DATA_BREAKPOINT: 0x");
        Serial.println(b, HEX);
        paused = true;
      }
    }

    unsigned long now = millis();
    if ((now - lastUpdate) > 1000) {
      //Serial.println(tickCount);
      lastUpdate = now;
    }
    

    if (tickCount == breakPoint) {
      paused = true;
      Serial.println("BREAKPOINT");
    }
    */
  }
  if (delay_us != 0) { delayMicroseconds(delay_us); }
  //if (delay_ms != 0) { delay(delay_ms); }
}

void checkSerial() {
  //Serial.println("CHECK SERIAL START");
  byte b;

  if (Serial.available() > 0) {
    b = Serial.read();
    b &= 0x7F;

    if (b == 't') { 
      tick(true);
      unsigned long ms = millis();
      Serial.print(tickCount);
      Serial.print(" ticks in ");
      Serial.print(ms);
      Serial.print("ms = ");
      Serial.print(tickCount/ms);
      Serial.println(" kHz.");
      Serial.flush();
    } else if (b == 'r') {
      reset();
    } else if (b == 'p') {
      paused = !paused;
    } else {
      cli();
      enqueue(outputQueue, b);
      sei();
    }
  }

  
  cli();
  if ((b = dequeue(inputQueue)) != EMPTY) {
    sei();
    //Serial.print("tty_out:");
    //Serial.println((char)byteToWrite);
    Serial.print((char)b);
    cli();
  }
  sei();

  //Serial.println("CHECK SERIAL END");
}

void reset() {
  Serial.println("RESETING...");
  digitalWrite(CLK, LOW);
  digitalWrite(RESET_, LOW);
  delay(100);
  tick(true);
  tick(true);
  tick(true);
  tick(true);
  delay(100);
  init_queue(inputQueue);
  init_queue(outputQueue);
  tickCount = 0;
  halted = false;
  start_millis = 0;
  digitalWrite(RESET_, HIGH);
  Serial.println("RUNNING...");
}

void loop() {
  if ((tickCount & 0xFF) == 0) {
    checkSerial();
  }
  
  if(digitalRead(HALT) == HIGH) {
      tick(false);
  } else if (!halted) {
      Serial.print("HALT@");
      Serial.println(tickCount);
      halted = true;
  }
}



void setup() {
  
  initOutput(LED, LOW);
  initOutput(RESET_, HIGH);
  initOutput(CLK, LOW);
  pinMode(HALT, INPUT);
  pinMode(TTYOUT_CP, INPUT);
  pinMode(TTYIN_CP, INPUT);
  pinMode(TTYIN_OE_, INPUT);

  Serial.begin(2000000);

  reset();
  
  cli();
  // https://thewanderingengineer.com/2014/08/11/arduino-pin-change-interrupts/
  PCMSK2 |= (1<<7); //PD7/PCINT23
  PCMSK2 |= (1<<6); //PD6/PCINT22        
  PCMSK2 |= (1<<5); //PD5/PCINT21
  PCICR |=  (1<<2); //PCINT2/PD
  sei();
}
