
#include <avr/interrupt.h>
#include <cppQueue.h>

#if defined(AVR_UNO)

const int
  RESET_ = 2,
  CLK = 3,
  HALT = 4,
  TTYOUT_CP = 5, //PD5, PCINT21
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
  TTYIN_RTR = 33,
  TTYOUT_CP = 10, //PB4, PCINT4
  TTYIN_OE_ = 12, //PB6, PCINT6
  LED = 13;

const int DATA_PINS[] = {
  22,23,24,25,26,27,28,29
};

#define DATA_PORT_IN PINA
#define DATA_PORT_OUT PORTA
#define DATA_PORT_DIR DDRA

#define PCICR_FLAGS (1<<0) //PCIE0/PB
#define PCMSK PCMSK0
#define PCMSK_FLAGS ((1<<6) /*PB6/PCINT6*/ | (1<<5) /*PB5/PCINT5*/ | (1<<4) /*PB4/PCINT4*/)
#define PCINT_vect PCINT0_vect

#define CLK_PORT PORTH
#define CLK_BIT 5

#endif


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

cppQueue  ttlToArduinoQueue(sizeof(char), 512, FIFO, false);
cppQueue  arduinoToTtlQueue(sizeof(char), 512, FIFO, false);

unsigned long tickCount = 0;
unsigned long lastBootMs = 0;
//#define BREAKPOINT (1120-INNER_TICKS)
bool paused = false;
bool halted = false;
bool debug = false;
#define DELAY_US  2
#define INNER_TICKS (4*8)


#if defined(BREAKPOINT)
#define UPDATE_TICKCOUNT_AND_CHECK_BREAKPOINT(ticks) do { if(tickCount < BREAKPOINT && tickCount + ticks >= BREAKPOINT) { paused = true;} tickCount += ticks; } while(0)
#else
#define UPDATE_TICKCOUNT_AND_CHECK_BREAKPOINT(ticks) do { tickCount += ticks; } while(0)
#endif

void initOutput(int pin, int initValue) {
  if (initValue != 0) {
    pinMode(pin, INPUT_PULLUP);
  } else {
    pinMode(pin, INPUT);
  }

  pinMode(pin, OUTPUT);
}

void setDataPinsInput() {
#if defined(DATA_PORT_OUT)
  DATA_PORT_DIR = 0x0;
  DATA_PORT_OUT = 0x0;
#else
  for(int i=0; i<8;i++) {
    pinMode(DATA_PINS[i], INPUT);
  }
#endif
}

void setDataPinsOutput() {
  byte b;
  if(arduinoToTtlQueue.pop(&b)) {
  } else {
    b = 0xFF;
  }

#if defined(DATA_PORT_OUT)
  DATA_PORT_DIR = 0xFF;
  DATA_PORT_OUT = b;
#else
  for(int i=0; i<8;i++) {
    initOutput(DATA_PINS[i], (b >> i) & 0x1);
  }
#endif

  if (arduinoToTtlQueue.getCount() == 0) {
    digitalWrite(TTYIN_RTR, LOW);
  }

  if (debug) {
    Serial.print("# CT->UC 0x");
    Serial.println(b, HEX);
  }
}

byte readDataPins() {
#if defined(DATA_PORT_OUT)
  return DATA_PORT_IN;
#else
  byte b = 0;
  for(int i=0; i<8;i++) {
    b |= digitalRead(DATA_PINS[i]) << i;
  }
  return b;
#endif
}

ISR(PCINT_vect){   // Port D, PCINT16 - PCINT23
  static bool prevTTYIN_OE_ = true;
  bool newTTYIN_OE_ = digitalRead(TTYIN_OE_);
  if (prevTTYIN_OE_ != newTTYIN_OE_) {
    if (newTTYIN_OE_ == HIGH) {
      setDataPinsInput();
    } else {
      setDataPinsOutput();
    }
    prevTTYIN_OE_ = newTTYIN_OE_;
  }
 
  static bool prevTTYOUT_CP = false;
  bool newTTYOUT_CP = digitalRead(TTYOUT_CP);
  if (!prevTTYOUT_CP && newTTYOUT_CP) {
    byte b = readDataPins();
    if (!ttlToArduinoQueue.push(&b)) {
      Serial.println("#ERR ttlToArduinoQueue overflow");
      halted = true;
    }
    if (debug) {
      Serial.print("# CT<-UC 0x");
      Serial.println(b, HEX);
    }
  }
  prevTTYOUT_CP = newTTYOUT_CP;
}

void halfTick() {
  CLK_PORT ^= (1<<CLK_BIT);
  delayMicroseconds(DELAY_US);
  UPDATE_TICKCOUNT_AND_CHECK_BREAKPOINT(1);
}

void tick(bool force) { 
  if (force || !paused) {
    for(int i=0; i < INNER_TICKS; i++) {
      CLK_PORT ^= (1<<CLK_BIT);
      delayMicroseconds(DELAY_US);
    }
    UPDATE_TICKCOUNT_AND_CHECK_BREAKPOINT(INNER_TICKS);
  }
}

void pushToTtlQueue(char c) {
  cli();
  while (!arduinoToTtlQueue.push(&c)) {
    sei();
    if (debug) {
      Serial.println("# waiting for TTL to dequeue...");
      Serial.flush();
    }
    delay(1000);
    cli();
  }
  digitalWrite(TTYIN_RTR, HIGH);
  sei();
}

void displayTicks() {
  unsigned long ms = millis() - lastBootMs;
  Serial.print(tickCount);
  Serial.print(" ticks in ");
  Serial.print(ms);
  Serial.print("ms = ");
  Serial.print(tickCount/ms);
  Serial.println(" kHz.");
  Serial.flush();
}

void checkSerial() {
  //Serial.println("CHECK SERIAL START");
  byte b;


  if (Serial.available() > 0) {
    b = Serial.read();
    if (b == 'h') {
      halfTick();
      displayTicks();
    } else if (b == 't') { 
      halfTick();
      halfTick();
      displayTicks();
    } else if (b == 'z') {
      reset();
    } else if (b == 'p') {
      paused = !paused;
    } else if (b == '?') {
      debug = !debug;
    } else if (b == 'y') {
      char *c = "s000F0EFC\nw00\nn\nw14\nn\nw08\nn\nw00\nq\n";
      while (*c) {
        pushToTtlQueue(*c);
        c++;
      }
    } else {
      if (debug) {
        Serial.print("# PC->CT: 0x");
        Serial.println(b, HEX);
      }
      pushToTtlQueue(b);
    }
  }

#if defined(ENABLE_WIFI)
  if (!client) {
    client = server.available();
  }
  
  if (client) {
    while (client.available() > 0) {
      b = client.read();
      cli();
      if (!arduinoToTtlQueue.push(&b)) {
        Serial.println("#ERR arduinoToTtlQueue overflow");
        halted = true;
      }
      sei();
    }
  }
#endif

  cli();
  if (ttlToArduinoQueue.pop(&b)) {
    sei();
    Serial.print((char)b);
#if defined(ENABLE_WIFI)
    if (client) {
      if (client.connected()) {
        client.write(b);
      }
    }
#endif
  } else {
    sei();
  }

  //Serial.println("CHECK SERIAL END");
}

void shutdown() {
  //Serial.println("RESETING...");
  digitalWrite(CLK, LOW);
  digitalWrite(RESET_, LOW);
  digitalWrite(TTYIN_RTR, LOW);
  delay(100);
  tick(true);
  tick(true);
  tick(true);
  tick(true);
  delay(100);
  ttlToArduinoQueue.clean();
  arduinoToTtlQueue.clean();
  halted = true;
}

void startup() {
  ttlToArduinoQueue.clean();
  arduinoToTtlQueue.clean();
  tickCount = 0;
  lastBootMs = millis();
  halted = false;
  digitalWrite(RESET_, HIGH);
  //Serial.println("RUNNING...");
}

void reset() {
  shutdown();
  startup();
}

void loop() {
  static uint16_t previousInCount = 0;
  static uint16_t previousOutCount = 0;

//  if (debug) {
//    uint16_t inCount = ttlToArduinoQueue.getCount();
//    if (previousInCount != inCount) {
//      Serial.print("#ttlToArduinoQueue length:");
//      Serial.println(inCount);
//      previousInCount = inCount;
//    }
//    
//    uint16_t outCount = arduinoToTtlQueue.getCount();
//    if (previousOutCount != outCount) {
//      Serial.print("#arduinoToTtlQueue length:");
//      Serial.println(outCount);
//      previousOutCount = outCount;
//    }
//  }

  checkSerial();
  
  for (int i=0; i< 16; i++) {
    //if ((DELAY_US == 65535) || (tickCount & 0xFF) == 0) {
    checkSerial();
    //}
    
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
  initOutput(RESET_, LOW);
  initOutput(CLK, LOW);
  initOutput(TTYIN_RTR, LOW);
  pinMode(HALT, INPUT);
  pinMode(TTYOUT_CP, INPUT);
  pinMode(TTYIN_OE_, INPUT);

  Serial.begin(2000000);

  if (!ttlToArduinoQueue.isInitialized()) {
    Serial.println("# ttlToArduinoQueue not initialized.");
    Serial.flush();
    return;
  }

  if (!arduinoToTtlQueue.isInitialized()) {
    Serial.println("# arduinoToTtlQueue not initialized.");
    Serial.flush();
    return;
  }

  shutdown();

#if defined(ENABLE_WIFI)
  memset(&wifiConnection, sizeof(wifiConnection), 0);
  //strcpy(wifiConnection.ssid, "{SSID}");
  //strcpy(wifiConnection.password, "{PASSWORD}");

  if (strlen(wifiConnection.password) != 0) {
    //update mode
    WiFiConnection existing;
    EEPROM.get(0, existing);
    if (0 == memcmp(&wifiConnection, &existing, sizeof(wifiConnection))) {
      Serial.println("# WiFi config already matches.");
    } else {
      EEPROM.put(0, wifiConnection);
      Serial.println("# Updating WiFi config.");
    }
  } else {
    EEPROM.get(0, wifiConnection);
    Serial.println("# Using existing WiFi config.");
  }

  Serial.print("# ");
  Serial.println(wifiConnection.ssid);
    
  Serial3.begin(115200);
  WiFi.init(Serial3);

  // Connect to WiFi
  while (WiFi.status() != WL_CONNECTED) {
    Serial.println("# Connecting to WiFi...");
    WiFi.begin(&wifiConnection.ssid[0], &wifiConnection.password[0]);
  }
  Serial.println("# Connected to WiFi");

  server.begin();
  
  // Print your WiFi shield's IP address
  IPAddress ip = WiFi.localIP();
  Serial.print("# IP Address: ");
  Serial.println(ip);
#endif

  reset();
  
  cli();
  // https://thewanderingengineer.com/2014/08/11/arduino-pin-change-interrupts/
  PCMSK |= PCMSK_FLAGS;
  PCICR |= PCICR_FLAGS;
  sei();
}
