# import the WFDB package
from serial import Serial
import numpy as np
import struct
import matplotlib.pyplot as plt
import time

def read_float_32(ser : Serial) -> float:
    data = bytearray()
    for i in range (4):
        b = ser.read(1)
        if b == 0x7D:
            next = ser.read(1)
            if next == (0x7D + 1):
                data.extend(0x3B)
            elif next == (0x7D + 2):
                data.extend(0x7D)
        elif b == b'' or b == 0x3B:
            return 0 # If we read a semicolon, we must be out of sync
        else:
            data.extend(b)
    return struct.unpack ('>f', bytes(data))[0]

def send_command(ser : Serial, cmd : str):
    #print(">" + cmd)
    ser.read_all()
    ser.reset_input_buffer()
    ser.reset_output_buffer()
    ser.write(("<"+cmd+">").encode())
    ser.flush()
    while not ser.readable:
        pass
    wait_response(ser)

def wait_response(ser : Serial):
    ser.read_until("<".encode())[:-1]
    data = ser.read_until(">".encode())[:-1]
    print("<" + data.decode(errors="ignore"))

if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, timeout=1) as ser:
        ser.reset_input_buffer()
        ser.reset_output_buffer()
      
        while True:
            user_input = input(">")
            send_command(ser, user_input.upper())
