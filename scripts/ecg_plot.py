import matplotlib.pyplot as plt
import ecg_com as ecg_com
import time
from serial import Serial
import numpy as np
import argparse
import re
import struct

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
        
        OUTPUT_FORMAT = "FLOAT32"
        SAMPLE_RATE = 1000
        SAMPLING_TIME = 5   # Seconds
        MAX_SAMPLE = int(SAMPLE_RATE * SAMPLING_TIME)

        values     = [0] * MAX_SAMPLE
        timestamps = [0] * MAX_SAMPLE
        peak_stamp = []

        ser.reset_input_buffer()
        ser.reset_output_buffer()

        ecg_com.send_command(ser, "STOP",                           True)
        ecg_com.send_command(ser, f"OUTPUT_FORMAT={OUTPUT_FORMAT}", True)
        ecg_com.send_command(ser, f"SAMPLE_RATE={SAMPLE_RATE}",     True)
        ecg_com.send_command(ser, "OUTPUT_STAGE=STAGE_RAW",         True)
        ecg_com.send_command(ser, "INPUT_CHANNEL=CH_ADC",         True)
        ecg_com.send_command(ser, "AMPLITUDE_COEF=1.5",             True)
        ecg_com.send_command(ser, "INPUT_GAIN=1.0",                 True)
        ecg_com.send_command(ser, "PEAK_DISTANCE=0.260",            True)
        ecg_com.send_command(ser, "WINDOW_SEC=0.15",                True)
        #ecg_com.send_command(ser, "ENABLE_TRIGGER=TRUE",            True)
        ecg_com.send_command(ser, "START",                          True)

        timestamp, value, is_peak = 0, 0, False
        for i in range(MAX_SAMPLE):

            if OUTPUT_FORMAT == "FLOAT32":
                timestamp = ecg_com.read_uint_32(ser)
                value = ecg_com.read_float_32(ser)
                is_peak = True if int(ser.read(1)[0]) == 1 else False
            else:
                rawdata = ecg_com.wait_response(ser)
                timestamp, value, peak = rawdata.split(";")
                is_peak = True if peak == 'TRUE' else False 

            values[i] = float(value)
            timestamps[i] = float(timestamp) * 0.001 # Convert ms in seconds
            
            if is_peak:
                peak_stamp.append(timestamps[i])
            
            print(timestamp, values[i], is_peak)

        ecg_com.send_command(ser, "STOP")

        plt.plot(timestamps, values, 'r')
        plt.xlabel("Time (s)")
        plt.ylabel("BPM or amplitude")

        for i in peak_stamp:
            plt.axvline(i)

       # plt.plot(np.convolve(values, np.ones(100)/100, mode="same"), 'b')

        plt.show()