# import the WFDB package
from serial import Serial
import numpy as np
import struct
import matplotlib.pyplot as plt
from prompt_toolkit import PromptSession
from prompt_toolkit.patch_stdout import patch_stdout 
import threading
import time
import re
import argparse

def read_uint_32(ser : Serial) -> int:
    return int(struct.unpack('>i', ser.read(4))[0])

def read_float_32(ser : Serial) -> float:
    #data = bytearray()
    # for i in range (4):
    #     b = ser.read(1)
    #     if b == 0x7D:
    #         next = ser.read(1)
    #         if next == (0x7D + 1):
    #             data.extend(0x3B)
    #         elif next == (0x7D + 2):
    #             data.extend(0x7D)
    #     elif b == b'' or b == 0x3B:
    #         return 0 # If we read a semicolon, we must be out of sync
    #     else:
    #         data.extend(b)
    return float(struct.unpack ('>f', ser.read(4))[0])

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
        #print("<" + response)
        
        if msg == None:
            break

    return response

def log(ser : Serial):
    while True: 
        if ser.in_waiting > 0:
            d = wait_response(ser)
            print("<" + d)
        else:
            time.sleep(0.1)

def valid_port(port):
    # Accepts formats like COM3 (Windows) or /dev/ttyUSB0 (Unix)
    if re.match(r"^COM\d+$", port) or re.match(r"^/dev/tty\w+$", port):
        return port
    else:
        raise argparse.ArgumentTypeError(f"Invalid port: {port}. Expected formats: COM3 or /dev/ttyUSB0")

def valid_baudrate(value):
    try:
        baud = int(value)
        if baud > 0:
            return baud
        else:
            raise ValueError
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid baudrate: {value}. Must be a positive integer.")


# Plot data coming from the ECG sensor
# This script can works if the board is running the main program (main.adb)
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Read and display an ECG signal coming from a board")
    parser.add_argument("-p", "--port", 
                        type=valid_port,
                        required=True,
                        help="Port to the board (/dev/ttyX on linux or COMX on windows)")
    parser.add_argument(
        "-b",
        "--baudrate",
        type=valid_baudrate,
        required=True,
        help="Baudrate for serial communication. Must be a positive integer, e.g., 9600 or 115200."
    )
   
    args = parser.parse_args()

    with Serial(args.port, args.baudrate, timeout=1) as ser:
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


