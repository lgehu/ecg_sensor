# import the WFDB package
from serial import Serial
import numpy as np
import struct

# Test Int16 transmissions to UART with the STM32F446RE
if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, write_timeout=1) as ser:
        #ser.write(b"SET_STATE=INIT;")
        #ser.write (b"OUTPUT_FORMAT=IEEE_FLOAT_32_BIGE;")
        ser.write (b"SET_STATE=SAMPLING;")
#        ser.read_until (b"OK") # OK\n\r

        for data in range(50):
            incoming = ser.read(4)
            rep = struct.unpack('>f', incoming)
            print('Sent: ', data, ', Received: ', incoming, rep[0])
            ser.read_until (b";")

