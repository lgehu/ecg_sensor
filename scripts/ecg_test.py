# import the WFDB package
from serial import Serial
import numpy as np
import struct
import matplotlib.pyplot as plt
from prompt_toolkit import PromptSession
from prompt_toolkit.patch_stdout import patch_stdout 
import threading
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

def send_command(ser : Serial, cmd : str, wait_ok : bool = False):
    #print(">" + cmd)
    ser.read_all()
    ser.reset_input_buffer()
    ser.reset_output_buffer()
    ser.write(("<"+cmd.upper()+">").encode())
    ser.flush()

    if wait_ok:
        wait_response(ser, "OK")

def wait_response(ser : Serial, msg : str | None = None):
    response = ""
    while response != msg:
        ser.read_until("<".encode())[:-1]
        data = ser.read_until(">".encode())[:-1]
        
        response = data.decode(errors="ignore")
        print("<" + response)
        
        if msg == None:
            break

    return data

def log(ser : Serial):
    while True: 
        if ser.in_waiting > 0:
            d = wait_response(ser)
        else:
            time.sleep(0.1)


if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, timeout=1) as ser:
        ser.reset_input_buffer()
        ser.reset_output_buffer()
      
        session = PromptSession()
        threading.Thread(target=log, args=(ser,), daemon=True).start()
        
        run = True

        while run:
            with patch_stdout():
                while True:
                    try:
                        user_input = session.prompt(">").upper()
                        send_command(ser, user_input)
                    except KeyboardInterrupt:
                        print("Terminating...")
                        run = False
                        break


