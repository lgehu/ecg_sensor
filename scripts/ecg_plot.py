import matplotlib.pyplot as plt
import ecg_com as ecg_com
import time
from serial import Serial
import numpy as np

if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, timeout=1) as ser:
        
        OUTPUT_FORMAT = "FLOAT32"
        MAX_SAMPLE = 10000

        values     = [0] * MAX_SAMPLE
        timestamps = [0] * MAX_SAMPLE
        pick_stamp = []

        ser.reset_input_buffer()
        ser.reset_output_buffer()

        ecg_com.send_command(ser, "STOP",                    True)
        ecg_com.send_command(ser, f"OUTPUT_FORMAT={OUTPUT_FORMAT}", True)
        ecg_com.send_command(ser, "SAMPLE_RATE=1000",        True)
        ecg_com.send_command(ser, "OUTPUT_STAGE=STAGE_HR",   True)
        ecg_com.send_command(ser, "INPUT_CHANNEL=CH_BTN",    True)
        ecg_com.send_command(ser, "AMPLITUDE_COEF=0.5",      True)
        ecg_com.send_command(ser, "INPUT_GAIN=1.0",          True)
        ecg_com.send_command(ser, "PICK_DISTANCE=0.260",     True)
        ecg_com.send_command(ser, "WINDOW_SEC=0.15",         True)
        #ecg_com.send_command(ser, "ENABLE_TRIGGER=TRUE", True)
        ecg_com.send_command(ser, "START",                   True)

        for i in range(MAX_SAMPLE):
            if OUTPUT_FORMAT == "FLOAT32":
                timestamps[i] = ecg_com.read_uint_32(ser)
                values[i] = ecg_com.read_float_32(ser)
                print(timestamps[i], values[i])
            else:
                rawdata = ecg_com.wait_response(ser)
                if rawdata != '' or rawdata.startswith("NaN"):
                    timestamp, value, is_pick = rawdata.split(";")
                    values[i] = float(value)
                    timestamps[i] = int(timestamp)

                    if is_pick == 'TRUE':
                        pick_stamp.append(i)

                print(timestamp, value, is_pick)

        ecg_com.send_command(ser, "STOP")

        plt.plot(values[500:], 'r')

        for i in pick_stamp:
            plt.axvline(i)

       # plt.plot(np.convolve(values, np.ones(100)/100, mode="same"), 'b')

        plt.show()