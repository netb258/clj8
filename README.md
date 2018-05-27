# CLJ8
Chip8 emulator written in Clojure.
Uses the Quil graphics library for rendering.

![Alt text](./screenshot.png?raw=true "Title")
![Alt text](./screenshot_2.png?raw=true "Title")
![Alt text](./screenshot_3.png?raw=true "Title")

# Usage
lein run PATH_TO_ROM

# Input
1 => 0x01
2 => 0x02
3 => 0x03
4 => 0x0C
Q => 0x04
W => 0x05
E => 0x06
R => 0x0D
A => 0x07
S => 0x08
D => 0x09
F => 0x0E
Z => 0x0A
X => 0x00
C => 0x0B
V => 0x0F
ENTER => Restart the emulator

# Credit
Credit goes to coldnew whose ClojureScript emulator is the basis for this emulator.
