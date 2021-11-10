
#include <avr/interrupt.h>

const int EMPTY = -256;

const int
  RESET_ = 2,
  CLK = 3, //chamge to 13 for LED?
  HALT = 4,
  TTYOUT_CP = 5, //PD5, PCINT21
  TTYIN_CP = 6, //PD6, PCINT22
  TTYIN_OE_ = 7; //PD7, PCINT23

const int DATA_PINS[] = {
  8,9,10,11,12,13,18,19
};

const int QUEUE_LENGTH = 64;
int inputQueue[QUEUE_LENGTH] = {0};
int outputQueue[QUEUE_LENGTH] = {0};

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
    Serial.println("TTYIN_OE_ LOW->HIGH");
  } else if (prevTTYIN_OE_ && !newTTYIN_OE_) {
    Serial.print("TTYIN_OE_ HIGH->LOW: 0x");
    byte b = readDataPins();
    Serial.print(b, HEX);
    Serial.println();
  }
  prevTTYIN_OE_ = newTTYIN_OE_;

  static bool prevTTYOUT_CP = false;
  bool newTTYOUT_CP = digitalRead(TTYOUT_CP);
  if (!prevTTYOUT_CP && newTTYOUT_CP) {
    byte b = readDataPins();
    enqueue(inputQueue, b);
    Serial.print("TTL->ARDUINO:");
    Serial.println(b);
  }
  prevTTYOUT_CP = newTTYOUT_CP;

  static bool prevTTYIN_CP = false;
  bool newTTYIN_CP = digitalRead(TTYIN_CP);
  if (!prevTTYIN_CP && newTTYIN_CP) {
    byte b = dequeue(outputQueue);
   // if (b > 0) {
      Serial.print("ARDUINO->TTL:");
      Serial.println(b);
    //}
  }
  prevTTYIN_CP = newTTYIN_CP;
}

unsigned long tickCount = 0;

void tick() { 
  digitalWrite(CLK, 1 ^ digitalRead(CLK));
  tickCount += 1;
  delayMicroseconds(10000);
}

void step() {
  cli();
  int byteToWrite = dequeue(inputQueue);
  sei();
  if (byteToWrite >= 0) {
    Serial.write((byte)byteToWrite);
  }

  tick();
}

void printQueue(int queue[QUEUE_LENGTH]) {
  for(int i=0; i < QUEUE_LENGTH; i++) {
    if (queue[i] == EMPTY) {
      Serial.print("{}");
    } else {
      Serial.print("0x");
      Serial.print(queue[i], HEX);
      Serial.print("=");
      Serial.print((char)queue[i]);
    }
    Serial.print(",");
  }
  Serial.println();
  Serial.flush();
}

void reset() {
  digitalWrite(CLK, LOW);
  digitalWrite(RESET_, LOW);
  delay(1000);
  tick();
  tick();
  tick();
  tick();
  digitalWrite(RESET_, HIGH);
}

void loop() {
  if (Serial.available() > 0) {
    byte b = Serial.read();
    b &= 0x7F;

    cli();
    Serial.print("PC->ARDUINO:");
    Serial.println(b);
    sei();
    Serial.flush();

    if (b == 't') { 
      tick();
      Serial.println(tickCount);
    } else if (b == 'r') {
      reset();
    } else {
      cli();
      enqueue(outputQueue, b);
      sei();
    }
  }
  
  if(digitalRead(HALT) == HIGH) {
    //if (tickCount < 2980) {
      step();
      //delay(1);
    //}
  }
  
//  if (inputQueue[0] != EMPTY) {
//    Serial.print("inputQueue: "); printQueue(inputQueue);
//  }
//  if (outputQueue[0] != EMPTY) {
//    Serial.print("outputQueue: "); printQueue(outputQueue);
//  }
}



void setup() {
  init_queue(inputQueue);
  init_queue(outputQueue);
  
  initOutput(CLK, LOW);
  initOutput (RESET_, HIGH);

  cli();
  // https://thewanderingengineer.com/2014/08/11/arduino-pin-change-interrupts/
  PCMSK2 |= (1<<7); //PD7/PCINT23
  PCMSK2 |= (1<<6); //PD6/PCINT22        
  PCMSK2 |= (1<<5); //PD5/PCINT21
  PCICR |=  (1<<2); //PCINT2/PD
  sei();

  Serial.begin(115200);
  
  Serial.println("RESETING...");
  reset();
}
