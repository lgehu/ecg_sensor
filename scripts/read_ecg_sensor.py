# import the WFDB package
from serial import Serial
import numpy as np
import struct
import matplotlib.pyplot as plt 

# Test Int16 transmissions to UART with the STM32F446RE
if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, write_timeout=1) as ser:
        # Send negative and positive values
        data = []
        while True:
            #ser.write([0x0F, 0xF8])
            raw = ser.read(2)
            value = struct.unpack('>h', raw)[0]
            
            print(f'{raw}={value}mV={float(value) / 1000}uV')
            
            if value == -666:
                break

            data.append(value)

        plt.plot([i for i in range(len(data))], data)
        plt.show()