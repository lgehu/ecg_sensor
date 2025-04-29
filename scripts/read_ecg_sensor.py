# import the WFDB package
from serial import Serial
import numpy as np
import struct

# Test Int16 transmissions to UART with the STM32F446RE
if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, write_timeout=1) as ser:
        # Send negative and positive values
        while True:
            #ser.write([0x0F, 0xF8])
            incoming = ser.read(2)
            rep = struct.unpack('>h', incoming)
            print(f'{rep}={incoming[0]}mV={float(incoming[0]) / 1000}uV')