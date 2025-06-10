import matplotlib.pyplot as plt
import ecg_test
import time
from serial import Serial

if __name__ == "__main__":
    # Open port (Linux only)
    with Serial("/dev/ttyACM0", baudrate=115200, timeout=1) as ser:
        ser.reset_input_buffer()
        ser.reset_output_buffer()

        ecg_test.send_command(ser, "STOP", True)
        ecg_test.send_command(ser, "OUTPUT_FORMAT=OUT_ASCII", True)
        ecg_test.send_command(ser, "SAMPLE_RATE=800", True)
        ecg_test.send_command(ser, "OUTPUT_STAGE=STAGE_DERIVATED", True)
        ecg_test.send_command(ser, "START", True)

        data = []
        for i in range(1000):
            #ecg_test.send_command(ser, "NEXT")
            value = ecg_test.wait_response(ser).decode()
            if value != '':
                data.append(float(value))

        plt.plot([i for i in range(len(data))], data)
        plt.show()

        ecg_test.send_command(ser, "STOP")