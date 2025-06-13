import matplotlib.pyplot as plt
import ecg_com as ecg_com
import time
from serial import Serial

if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, timeout=1) as ser:
        
        MAX_SAMPLE = 10000 

        values = [0] * MAX_SAMPLE
        timestamps = [0] * MAX_SAMPLE

        ser.reset_input_buffer()
        ser.reset_output_buffer()

        ecg_com.send_command(ser, "STOP", True)
        ecg_com.send_command(ser, "OUTPUT_FORMAT=OUT_ASCII", True)
        ecg_com.send_command(ser, "SAMPLE_RATE=100", True)
        ecg_com.send_command(ser, "OUTPUT_STAGE=STAGE_INTEGRATED", True)
        ecg_com.send_command(ser, "PICK_DISTANCE=0.6", True)
        ecg_com.send_command(ser, "START", True)

        for i in range(MAX_SAMPLE):
            rawdata = ecg_com.wait_response(ser)
            if rawdata != '':
                timestamp, value = rawdata.split(";")
                values[i] = float(value)
                timestamps[i] = int(timestamp)
                print(timestamp, value)

        ecg_com.send_command(ser, "STOP")

        plt.plot(timestamps, values)
        plt.show()