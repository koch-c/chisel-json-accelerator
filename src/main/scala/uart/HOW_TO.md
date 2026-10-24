The UART component works and has been tested on the board. `./Uart.xdc` was used as a design constraint. 

Program the board and connect to the UART on the board with: 
```sh
picocom -b 115200 --flow n --parity n --databits 8 --stopbits 1 /dev/ttyUSB1
```
Ensure `picocom` echo is disabled. 

It works if when you press a key, the key appears in the terminal. As if you are writing normally. Try and press the reset button (center button) on the board and type something. When doing this, and you press a key, nothing should occur.
