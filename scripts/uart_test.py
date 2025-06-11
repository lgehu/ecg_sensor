# import the WFDB package
from serial import Serial
import numpy as np
import struct

# Test Int16 transmissions to UART with the STM32F446RE
# This script can works if the board is running the program test/uart_test.adb.
if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, write_timeout=1) as ser:
        # Send negative and positive values
        for data in [-2**15, 10000, 100, 0, 100, 10000, 2**15 - 1]:
            #ser.write([0x0F, 0xF8])
            ser.write(struct.pack('>h', data)) # Integer to Big Indian 16 bits
            
            incoming = ser.read(2)
            rep = struct.unpack('>h', incoming)
            print('Sent: ', data, ', Received: ', incoming, rep[0])