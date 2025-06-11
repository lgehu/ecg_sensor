# import the WFDB package
from serial import Serial
import wfdb
import numpy as np
import struct
import matplotlib.pyplot as plt
import argparse
import os
import re

def valid_port(port):
    # Accepts formats like COM3 (Windows) or /dev/ttyUSB0 (Unix)
    if re.match(r"^COM\d+$", port) or re.match(r"^/dev/tty\w+$", port):
        return port
    else:
        raise argparse.ArgumentTypeError(f"Invalid port: {port}. Expected formats: COM3 or /dev/ttyUSB0")

def valid_file(path):
    if os.path.exists(path + ".hea") and os.path.isfile(path + ".hea"):
        return path
    else:
        raise argparse.ArgumentTypeError(f"File does not exist or is not a valid file: {path}")

def valid_baudrate(value):
    try:
        baud = int(value)
        if baud > 0:
            return baud
        else:
            raise ValueError
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid baudrate: {value}. Must be a positive integer.")

def arg_parse():
    parser = argparse.ArgumentParser(
        description="Program to configure a serial communication and load a study file."
    )

    parser.add_argument(
        "-p",
        "--port",
        type=valid_port,
        required=True,
        help="Serial port to use. Expected format: COMx (Windows) or /dev/ttyX (Linux/macOS)."
    )

    parser.add_argument(
        "-b",
        "--baudrate",
        type=valid_baudrate,
        required=True,
        help="Baudrate for serial communication. Must be a positive integer, e.g., 9600 or 115200."
    )

    parser.add_argument(
        "-f",
        "--file",
        type=valid_file,
        required=True,
        help="Path to the study file. File must exist."
    )

    args = parser.parse_args()

    print(f"Selected serial port: {args.port}")
    print(f"Baudrate: {args.baudrate}")
    print(f"Study file: {args.file}")

    return( args)

# This script send ECG signals to the board and diplay the processed result from the board.
# This script can works if the board is running the program test/ecg_test.adb.
if __name__ == "__main__":
    args = arg_parse()

    path = args.file

    signals, fields = wfdb.rdsamp(path, channels=[0])
    data = []

    print(fields)

    for sig in signals:
        data.append(sig[0] * 1000) # Convert to uV

    timeStamp = [i/fields["fs"] for i in range(len(data))]
    plt.plot([i/100 for i in range(0, len(data))], data)
    plt.title('Normal ECG')
    plt.xlabel('Temps (s)')
    plt.ylabel('uV')
    plt.grid(True)
    plt.show()

    stm32_result = []

    with Serial(args.port, args.baudrate, write_timeout=1) as ser:
        # Sending to port
        for d in data:
            ser.write(struct.pack('>h', int(d)))
            rep = struct.unpack('>h', ser.read(2))
            print("> Sent:", d, "Received:", rep[0])
            stm32_result.append(rep[0])

    plt.plot([i for i in range(len(stm32_result))], stm32_result)
    plt.grid(True)
    plt.show()
# plot the record to screen
#wfdb.plot_wfdb(record=record, title='Example signals')