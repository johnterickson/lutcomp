
#include <avr/interrupt.h>
#include <cppQueue.h>

#if defined(AVR_UNO)

const int
  RESET_ = 2,
  CLK = 3,
  HALT = 4,
  TTYOUT_CP = 5, //PD5, PCINT21
  TTYIN_CP = 6,  //PD6, PCINT22
  TTYIN_OE_ = 7, //PD7, PCINT23
  LED = 13;

const int DATA_PINS[] = {
  8,9,10,11,14,15,16,17
};

#define PCICR_FLAGS (1<<2) //PCINT2/PD
#define PCMSK PCMSK2
#define PCMSK_FLAGS ((1<<7) /*PD7/PCINT23*/ | (1<<6) /*PD6/PCINT22*/ | (1<<5) /*PD5/PCINT21*/)
#define PCINT_vect PCINT2_vect

#define CLK_PORT PORTD
#define CLK_BIT 3

#elif defined(ARDUINO_AVR_MEGA2560)

#define ENABLE_WIFI

const int
  RESET_ = 7,
  CLK = 8,
  HALT = 9,
  TTYOUT_CP = 10, //PB4, PCINT4
  TTYIN_CP = 11,  //PB5, PCINT5
  TTYIN_OE_ = 12, //PB6, PCINT6
  LED = 13;

const int DATA_PINS[] = {
  22,23,24,25,26,27,28,29
};

#define PCICR_FLAGS (1<<0) //PCIE0/PB
#define PCMSK PCMSK0
#define PCMSK_FLAGS ((1<<6) /*PB6/PCINT6*/ | (1<<5) /*PB5/PCINT5*/ | (1<<4) /*PB4/PCINT4*/)
#define PCINT_vect PCINT0_vect

#define CLK_PORT PORTH
#define CLK_BIT 5

#endif

#undef ENABLE_WIFI

#if defined(ENABLE_WIFI)
#include <EEPROM.h>
#include "WiFiEspAT.h"

struct WiFiConnection {
  char ssid[32];
  char password[32];
};

WiFiConnection wifiConnection;

WiFiServer server(23);
WiFiClient client;
#endif

const int QUEUE_LENGTH = 64;
cppQueue  inputQueue(sizeof(char), QUEUE_LENGTH, FIFO, false);
cppQueue  outputQueue(sizeof(char), QUEUE_LENGTH, FIFO, false);

unsigned long tickCount = 0;
//unsigned long breakPoint = 0;//1700/4*4;
//long dataBreakPoint = 0x23;
bool paused = false;
bool halted = false;
#define DELAY_US  2
#define INNER_TICKS (4*8)

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
    byte b;
    if(outputQueue.peek(&b)) {
      b |= 0x80;
    } else {
      b = 0;
    }
    initOutput(DATA_PINS[i], (b >> i) & 0x1);
  }
}



byte readDataPins() {
  byte b = 0;
  for(int i=0; i<8;i++) {
    b |= digitalRead(DATA_PINS[i]) << i;
  }
  return b;
}

ISR(PCINT_vect){   // Port D, PCINT16 - PCINT23
  bool newTTYIN_OE_ = digitalRead(TTYIN_OE_);
  if (newTTYIN_OE_ == HIGH) {
    setDataPinsInput();
  } else {
    setDataPinsOutput();
  }
 
  static bool prevTTYOUT_CP = false;
  bool newTTYOUT_CP = digitalRead(TTYOUT_CP);
  if (!prevTTYOUT_CP && newTTYOUT_CP) {
    byte b = readDataPins();
    if (b != 0) {
      inputQueue.push(&b);
    }
  }
  prevTTYOUT_CP = newTTYOUT_CP;

  static bool prevTTYIN_CP = false;
  bool newTTYIN_CP = digitalRead(TTYIN_CP);
  if (!prevTTYIN_CP && newTTYIN_CP) {
    byte dequeued;
    if (!outputQueue.pop(&dequeued)) {
      //Serial.print("TTL->ARDUINO ACK: Nothing to dequeue.");
    } else {
      dequeued &= 0x7f;
      byte acked = readDataPins() & 0x7f;
      if (dequeued != acked) {
        Serial.print("TTL->ARDUINO ACK: 0x");
        Serial.print(dequeued, HEX);
        Serial.print(" vs 0x");
        Serial.println(acked, HEX);
      }
    }
  }
  prevTTYIN_CP = newTTYIN_CP;
}


void tick(bool force) { 
  if (force || !paused) {
    for(int i=0; i < INNER_TICKS; i++) {
      CLK_PORT ^= (1<<CLK_BIT);
      delayMicroseconds(DELAY_US);
      tickCount += 1;
    }
  }
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
    } else if (b == 'z') {
      reset();
    } else if (b == 'p') {
      paused = !paused;
    } else {
      /*
      Serial.print("#PC->Arduino @");
      Serial.print(millis());
      Serial.print(": ");
      Serial.println(b, HEX);
      */
      cli();
      outputQueue.push(&b);
      sei();
    }
  }

#if defined(ENABLE_WIFI)
  WiFiClient client = server.available();
#endif

  cli();
  if (inputQueue.pop(&b)) {
    sei();

    /* 
    static char last_char = 0;
  
    if (last_char == '\n') {
      Serial.print("#Arduino->PC @");
      Serial.print(millis());
      Serial.print(": ");
      Serial.println(b, HEX);
    }
    */
    if (b > 0) {
      Serial.print((char)b);
      //last_char = (char)b;
    }
    cli();
  }
  sei();

  //Serial.println("CHECK SERIAL END");
}

void reset() {
  //Serial.println("RESETING...");
  digitalWrite(CLK, LOW);
  digitalWrite(RESET_, LOW);
  delay(100);
  tick(true);
  tick(true);
  tick(true);
  tick(true);
  delay(100);
  inputQueue.clean();
  outputQueue.clean();
  tickCount = 0;
  halted = false;
  digitalWrite(RESET_, HIGH);
  //Serial.println("RUNNING...");
}

void loop() {
  for (int i=0; i< 4; i++) {
    if ((tickCount & 0xFF) == 0) {
      checkSerial();
    }
    
    if(digitalRead(HALT) == HIGH) {
        tick(false);
    } else if (!halted) {
        unsigned long start = millis();
        do {
          checkSerial();
        } while ((millis() - start) < 1000);
        Serial.print("HALT@");
        Serial.println(tickCount);
        halted = true;
    }
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

#if defined(ENABLE_WIFI)
  memset(&wifiConnection, sizeof(wifiConnection), 0);
  //strcpy(wifiConnection.ssid, "{SSID}");
  //strcpy(wifiConnection.password, "{PASSWORD}");

  if (strlen(wifiConnection.password) != 0) {
    //update mode
    WiFiConnection existing;
    EEPROM.get(0, existing);
    if (0 == memcmp(&wifiConnection, &existing, sizeof(wifiConnection))) {
      Serial.println("WiFi config already matches.");
    } else {
      EEPROM.put(0, wifiConnection);
      Serial.println("Updating WiFi config.");
    }
  } else {
    EEPROM.get(0, wifiConnection);
    Serial.println("Using existing WiFi config.");
  }
  Serial.println(wifiConnection.ssid);
  //Serial.println(wifiConnection.password);

  
  Serial3.begin(115200);
  WiFi.init(Serial3);

  // Connect to WiFi
  while (WiFi.status() != WL_CONNECTED) {
    Serial.println("Connecting to WiFi...");
    WiFi.begin(&wifiConnection.ssid[0], &wifiConnection.password[0]);
  }
  Serial.println("Connected to WiFi");

  server.begin();
  
  // Print your WiFi shield's IP address
  IPAddress ip = WiFi.localIP();
  Serial.print("IP Address: ");
  Serial.println(ip);
#endif

  reset();
  
  cli();
  // https://thewanderingengineer.com/2014/08/11/arduino-pin-change-interrupts/
  PCMSK |= PCMSK_FLAGS;
  PCICR |=  PCICR_FLAGS;
  sei();
}
