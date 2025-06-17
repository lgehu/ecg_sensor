import matplotlib.pyplot as plt
import ecg_com as ecg_com
import time
from serial import Serial
import numpy as np

if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, timeout=1) as ser:
        
        MAX_SAMPLE = 12000

        values     = [0] * MAX_SAMPLE
        timestamps = [0] * MAX_SAMPLE
        pick_stamp = []

        ser.reset_input_buffer()
        ser.reset_output_buffer()

        ecg_com.send_command(ser, "STOP",                    True)
        ecg_com.send_command(ser, "OUTPUT_FORMAT=OUT_ASCII", True)
        ecg_com.send_command(ser, "SAMPLE_RATE=1000",        True)
        ecg_com.send_command(ser, "OUTPUT_STAGE=STAGE_HR",   True)
        ecg_com.send_command(ser, "INPUT_CHANNEL=CH_FLASH",   True)
        ecg_com.send_command(ser, "AMPLITUDE_COEF=2",      True)
        ecg_com.send_command(ser, "PICK_DISTANCE=0.260",     True)
        ecg_com.send_command(ser, "START",                   True)

        for i in range(MAX_SAMPLE):
            rawdata = ecg_com.wait_response(ser)
            if rawdata != '' or rawdata.startswith("NaN"):
                timestamp, value, is_pick = rawdata.split(";")
                values[i] = float(value)
                timestamps[i] = int(timestamp)

                if is_pick == 'TRUE':
                    pick_stamp.append(i)

                print(timestamp, value, is_pick)

        ecg_com.send_command(ser, "STOP")

        timestamps = [i for i in range(MAX_SAMPLE)]

        plt.plot(values[500:], 'r')

        for i in pick_stamp:
            plt.axvline(i)

       # plt.plot(np.convolve(values, np.ones(100)/100, mode="same"), 'b')

        plt.show()