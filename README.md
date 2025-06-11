# ECG SENSOR #
The goal is to virtualize a sensor in the STM32F446RE that can process ECG data to detect heart rate with the Pan-Tompkins algorithm.
Data are transmit through UART with a python script, then, the MCU process the data in real time and should return the heart rate.

## SCRIPTS ##
- ecg_com.py : Interactive command prompt that send and receive commands to the ECG Sensor. You must build the main program to communicate with the sensor.
- ecg_plot.py : Example script that automatically configure the sensor and plot processed ecg signal. Works 
- uart_test.py: Is intented for testing the UART and transmist signed integer on 16 bits.  
- filters_proto.py: Is for testing algorithm of filter to be implemented on Ada.  
- read_ecg_sensor.py: Read an ECG signal from the board and display it 
- ecg_uart_test.py: Is for sending a whole ECG signal. 
to_ada.py: Is for converting any file to an ada array

## COMMANDS ##
To exchange commands to the sensor, you first need to build the main project. Once, you can use any terminal program which support UART.  
You can use screen or minicom on linux or termite on Windows. The easiest way would be to use the dedicated script "ecg_com" to send commands.  

### Protocol ##
The protocol is really simple and use ASCII format case sensitive. The caracteres '<' and '>' are start and terminating flags.  
Every commands are stored in the board as a parameter on a KEY=VALUE format.
A command can be either an action or a parameter depending on the formatting.
Example:  
Sending <GET_ARGS> without value return all commands and parameter registered in the board.  
Sending <GET_ARGS=SAMPLE_RATE> return the current stored value linked to the key 'SAMPLE_RATE'.  
Sending <SAMPLE_RATE=> with the sign '=' and without value reset the parameter to his default value.
If the command is either misswritten or does not exist, the board respond with an error. 
The command interpreter on the board side perform a type check on reception before storing the new value.  
It mean that no value will be updated if you send a value of incorrect format or out of bounds. An error is sent on error.
Example: <SAMPLE_RATE=100.5> send an error because a Natural (All integer greater than 0) is expected.

**General commands**    
| Commands/Parameters |  Description  | Argument type
| :---:               | ------------- | :---:   |    
| GET_ARGS            | Return all arguments with format KEY=VALUE\r\n or the the value of the specified key if provided. | String |
| OUTPUT_FORMAT       | Set the output format of processed data. On ASCII mode, the format is <float_value>. On binary mode, 4 bytes is sent in Big endian format. Each sample are separated with the caractere ';'. Thus, data can extra byte in case of escape value. | OUT_ASCII &#124; FLOAT32 |
| RESET               | Restart the board | None |
| START               | Start automatic sampling on the selected input channel (default from flash) and send back result with the selected output format. During sampling, some parameters of the ECG sensor may not be applied. | None |  
| STOP                | Stop automatic sampling. Reset the sample index to 0. | None |
| PAUSE               | Stop sampling and keep the actual sample index if the input channel is the FLASH. To resume sampling, send a START command. | None |
| NEXT                | Request a single sample. No need to start sampling. | None |
| VERSION             | Ask for the ecg version. | None |
| SAMPLE_RATE         | Set the output frequency during the automatic sampling. | 0 < Integer_Value |

**ECG commands**  
 Commands/Parameters |  Description  | Argument type
| :---:              | ------------- | :---:   |    
| AMPLITUDE_COEF     | Set the multiplier for the amplitude threshold. This treshold is the mean of the integrated data during the Pan-Tompkins algorithm. | 0.0 < Float_Value < 2.0  |
| PICK_DISTANCE      | Set the minimal time distance in second between to pick. | 0.0 < Float_Value |
|  WINDOW_SEC        | Moving window length in second during the integration stage. | 0.0 < Float_Value |
| OUTPUT_STAGE       | Set the ouput stage during the Pan-Tompkins algorithm. The last stage return the heart rate  (HR). | Stage_Filtered &#124; Stage_Derivatived &#124; Stage_Squared &#124; Stage_Integrated &#124; Stage_HR |

## PREREQUISITES (Linux) ##
You will need Alire, st-flash, python3 and the right toolchain for Ada (gnat-arm-elf).  
```bash
wget https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-x86_64-linux.zip
unzip alr-2.0.2-bin-x86_64-linux.zip
mv alr-2.0.2-bin-x86_64-linux/bin /usr/bin/alr`
alr toolchain --select gnat_arm_elf=14.2.1 gprbuild=22.0.1
```
Then, download the ADL fork for Alire beside this project:   
```bash
git clone https://github.com/lgehu/alr_adl_crates.git
```
Your_folder   
│  
├── ecg_sensor  
├── alr_adl_crates  
    
## COMPILATION ##
Plug your device to your computer, then to compile the Ada project run:   
`make PRJ_NAME=ecg_test`
You can replace 'ecg_test' with an other file in the src/test folder (without extension).  
You should change in the makefile the port of your device if needed.

## Flashing an ECG file to the board ## 

### Testing with object linker 

Create an object file to be linked to the program.
```bash
alr exec -- arm-eabi-objcopy -I binary -O elf32-littlearm -B arm --rename-section .data=.rodata test.txt obj/test.o
```
Or,
```bash 
alr exec -- ld -r -b binary --section-start=.rodata=0x0807F000 -o obj/test.o test.txt
```

To show the generated symboles names, we can print them with:
```bash
alr exec -- arm-eabi-nm obj/test.o
```

To print the content of the file:
```bash
alr exec -- arm-eabi-objdump -s -j .rodata obj/test.o
```

In the GPR add the following lines:
```Ada
package Linker is
      for Linker_Options use ("obj/test.o");
end Linker;

```
In Ada, we can then load the symboles:
```Ada
   Fichier_Start : System.Address;
   pragma Import (C, Fichier_Start, "_binary_test_txt_start");
   Fichier_End : System.Address;
   pragma Import (C, Fichier_End, "_binary_test_txt_end");
```
This doesn't work. First, the only way we found to link an object the binary is by adding an argument to command line ( -largs obj/test.o). Linker options added in the GPR didn't work.
Once the exernal file linked, the addresses Text_Start and Text_End in the Ada program were not acceptable. For example we had sometime negative value, or Text_Start higher than Text_End. The issue could be during the "ld", dealing bad with these external symbols.

### Solution with a script
In the script folder, the file to_ada.py take an arbitrary file and generate an array of bytes in the Ada syntax. This way, we can embbed any data during the compilation.
The script can be used like this:
```bash 
python3 scripts/to_ada.py physionet.org/files/ptb-xl/1.0.3/records100/00000/00001_lr src/ecgdata.ads ECGData --wfdb
```
Execution of the script that allows to convert a .dat dataset into a Ada spec file [Package name]. For our example we used ECGData.
It will generate an array of float32. Then, in the `ecg_script_test.adb`, we iterate through the array, convert it to Int16 and send it using UART at a given sample rate.
`read_ecg_sensor.py` will acquire data and convert it back to float.

# Pan-Tompkins
We implemented the Pan-Tompkins algorithm on the STM32F446RE. According to the paper, it filter the input data with a pass-band from 5 Hz to 15 Hz. Currently, its a simple IIR filter, but we could enhance it by using the DSP  and CMSIS library to implement a butterworth in some future. Then, it square the signal and perform a moving window to smooth the signal. Finally, to detect the pick, it compute one threshold based on the average amplitude, and the second treshold is a minimal distance between picks.

## Demo
The ecg_test.adb program use an ECG signal generated by the to_ada.py script. Then, results of the processed signal is sent through UART and plotted to be compared with the Pan-Tompkins python version. 
On the following picture, the first one is an ECG signal processed by the python algorithm from this [repo](https://github.com/lgehu/Pan-Tompkins-algorithm-python.git).   
![python-algo](https://github.com/user-attachments/assets/b64c858f-ec68-40bc-9dfc-82cd7c9bac8a)  
The next one is the same ECG signal processed by the Ada implementation. We can see similar results: No false positive, same picks number and detected on the same time.
However, amplitude are different because lowpass and highpass are IIR functions instead of butterworth. Thus, gain response are not the same. 
![ada-algo](https://github.com/user-attachments/assets/8a5ad799-4a83-48e4-911f-79fcc074056d)  

## How to use
Here is the following script and command we used to get the precedent results.  
First, transform an ECG signal into an ada array: `python3 scripts/to_ada.py physionet.org/files/ptb-xl/1.0.3/records100/00000/00001_lr src/ecgdata.ads ECGData --wfdb`  
Next, compile the program and flash it: `make PRJ_NAME=ecg_test`  
If you only want the raw data outputted by the board, use: `make PRJ_NAME=ecg_script_test`  
It will send the raw signal casted to Int16 through UART.  
Then, execute the python script to display the result: `python3 scripts/read_ecg_sensor.py`  
You can restart the board, it will send the data in a singleshot.

# TODO #
- [x] Implement the Pan-Tompkins algorithm
- [x] Python script to flash an ECG signal directly in the board.
- [ ] Make the solution with object linking working 
- [ ] Enhance the algorithm using the DSP and CMSIS Library

# ISSUES #
Python package wfdb and matplotlib wasn't working fine on linux. I had to create a
pyenv:   
`python3 -m venv`   
`source ecg_sensor/bin/activate`
Then,   
`pip install wfdb matplotlib pystruct pyserial`
