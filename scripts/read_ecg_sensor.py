# import the WFDB package
from serial import Serial
import numpy as np
import struct
import matplotlib.pyplot as plt 
import argparse
import re

def valid_port(port):
    # Accepts formats like COM3 (Windows) or /dev/ttyUSB0 (Unix)
    if re.match(r"^COM\d+$", port) or re.match(r"^/dev/tty\w+$", port):
        return port
    else:
        raise argparse.ArgumentTypeError(f"Invalid port: {port}. Expected formats: COM3 or /dev/ttyUSB0")

# Test Int16 transmissions to UART with the STM32F446RE
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Read and display an ECG signak coming from a board")
    parser.add_argument("-p", "--port", 
                        type=valid_port,
                        required=True,
                        help="Port to the board (/dev/ttyX on linux or COMX on windows)")
    args = parser.parse_args()

    with Serial(args.port, baudrate=115200, write_timeout=1) as ser:
        data = []
        while True:
            raw = ser.read(2)
            value = struct.unpack('>h', raw)[0]
            
            print(f'{raw}={value}mV={float(value) / 1000}uV')
            
            if value == -666:
                break

            data.append(value)

        plt.plot([i for i in range(len(data))], data)
        plt.xlabel("Time (ms)")
        plt.ylabel("Amplitude (uV)")
        plt.title("ECG picks detection computed by the board")
        plt.show()