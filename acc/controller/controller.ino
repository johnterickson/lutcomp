
const int
  RESET_ = 2,
  CLK = 3,
  HALT = 4;

void initOutput(int pin, int initValue) {
  if (initValue != 0) {
    pinMode(pin, INPUT_PULLUP);
  } else {
    pinMode(pin, INPUT);
  }

  pinMode(pin, OUTPUT);
}

void setup() {
  initOutput(CLK, LOW);
  initOutput (RESET_, HIGH);
  
  digitalWrite(RESET_, LOW);
  delay(1000);
  digitalWrite(RESET_, HIGH);
}

void loop() {
  digitalWrite(CLK, 1 ^ digitalRead(CLK));
  delay(100);
}
