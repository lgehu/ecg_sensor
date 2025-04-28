# ECG SENSOR #
The goal is to virtualize a sensor in the STM32F446RE that can process ECG data to detect heart rate with the Pan-Tompkins algorithm.
Data are transmit through UART with a python script, then, the MCU process the data in real time and should return the heart rate.

## SCRIPTS ## 
uart_test.py: Is intented for testing the UART and transmist signed integer on 16 bits. <br/>
filters_proto.py: Is for testing algorithm of filter to be implemented on Ada. <br/>
ecg_uart_test.py: Is for sending a whole ECG signal. 

## PREREQUISITES (Linux) ##
You will need Alire, st-flash, python3 and the right toolchain for Ada (gnat-arm-elf).<br/>
Download the package manager for Ada (Alire): <\br>
```bash
wget https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-x86_64-linux.zip
unzip alr-2.0.2-bin-x86_64-linux.zip
mv alr-2.0.2-bin-x86_64-linux/bin /usr/bin/alr`
alr toolchain --select gnat_arm_elf=14.2.1 gprbuild=22.0.1
```
Then, download the ADL fork for Alire beside this project: <br/>
```bash
git clone https://github.com/lgehu/alr_adl_crates.git
```
Your_folder <br/>
│ </br>
├── ecg_sensor <br/>
├── alr_adl_crates
    
## COMPILATION ##
Plug your device to your computer, then to compile the Ada project run: <br/>
`make PRJ_NAME=ecg_test`
You can replace 'ecg_test' with an other file in the src/test folder.<br/>
You should change in the makefile the port of your device if needed.

## Flashing an ECG file to the board ## 

### Testing with linker 

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

# TODO #
- [ ] Implement the Pan-Tompkins algorithm (In standby)
- [ ] Python script to flash an ECG signal directly in the board. 

# ISSUES #
Python package wfdb and matplotlib wasn't working fine on linux. I had to create a
pyenv: <br/>
`python3 -m venv` <br/>
`source ecg_sensor/bin/activate`
Then, <br/>
`pip install wfdb matplotlib pystruct pyserial`
