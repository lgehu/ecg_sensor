import wfdb
import matplotlib.pyplot as plt
import math
import numpy as np 

def lowpass_filter(sig, fs, fc, order=1, gain=1):
    alpha = 1 / (1 + fs / (2 * math.pi * fc))
    last_result = sig
    result = []
    
    for _ in range(order):
        result = []
        last_output = last_result[0]
        for i in last_result:
            y = (1 - alpha) * last_output + gain * alpha * i
            result.append(y)
            last_output = y
        last_result = result

    return result

def highpass_filter(sig, fs, fc, order=1, gain=1):
    alpha = fs / (fs + 2 * math.pi * fc)
    last_result = sig
    result = []

    for _ in range(order):
        result = []
        last_output = last_result[0]
        last_input = last_result[0]
        for i in last_result:
            y = alpha * (last_output + gain * (i - last_input))
            result.append(y)
            last_output = y
            last_input = i
        last_result = result

    return result

def bandpass_filters(sig, fs, fc1, fc2, order=1, gain=1):
    output = lowpass_filter(sig, fs, fc1, order, gain)
    output = highpass_filter(output, fs, fc2, order, gain)
    return output

def measure_gain(fs, f_test, filter, *arg):
    gains_db = []
    t = np.arange(0, 5.0, 1.0/fs)
    for freq in f_test:
        input_signal = np.sin(2 * np.pi * freq * t)

        output_signal = filter(input_signal, *arg)    
        input_signal = input_signal[len(input_signal)//2:]
        output_signal = output_signal[len(output_signal)//2:]

        gain = np.max(np.abs(output_signal)) / np.max(np.abs(input_signal))
        gain_db = 20 * np.log10(gain)
        gains_db.append(gain_db)
    
    return gains_db

if __name__ == "__main__":

    # Testing filters responses(lowpass, highpass, bandpass)
    fc1, fc2 = 30, 20
    gain = 1
    fs = 10000
    order = 1
    freqs = [1, 2, 5, 10, 15, 20, 50, 100, 200, 500, 1000]
    gains_db = measure_gain(fs, freqs, bandpass_filters, fs, fc1, fc2, order, gain)
    
    plt.semilogx(freqs, gains_db, label=f"Ordre {order}")
    plt.axvline(fc1, color='gray', linestyle='--', label="Fc1")
    plt.axvline(fc2, color='gray', linestyle='--', label="Fc2")
    plt.title("Mesure du gain du filtre passe-bas")
    plt.xlabel("Fréquence (Hz)")
    plt.ylabel("Gain (dB)")
    plt.grid(True, which="both", linestyle="--", alpha=0.5)
    plt.legend()
    plt.show()

    # Testing filters with real ECG signals
    signals, fields = wfdb.rdsamp('physionet.org/files/ptb-xl/1.0.3/records100/00000/00003_lr', channels=[0])
    ecg_data = [i[0] * 1000 for i in signals] # Convert to uV
    timeStamp = [i/fields["fs"] for i in range(len(ecg_data))]
    ecg_filtered = bandpass_filters(ecg_data, 100, 15, 5, 2, 1.0)
    fig, (ax1, ax2) = plt.subplots(2)
    
    ax1.plot(timeStamp, ecg_data)
    ax2.plot(timeStamp, ecg_filtered)

    plt.show()