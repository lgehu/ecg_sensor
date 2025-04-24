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
`wget https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-x86_64-linux.zip`
`unzip alr-2.0.2-bin-x86_64-linux.zip`
`mv alr-2.0.2-bin-x86_64-linux/bin /usr/bin/alr`
`alr toolchain --select gnat_arm_elf=14.2.1 gprbuild=22.0.1`<br/>
Then, download the ADL fork for Alire beside this project: <br/>
`git clone https://github.com/lgehu/alr_adl_crates.git`<br/>
Your_folder <br/>
│ </br>
├── ecg_sensor <br/>
├── alr_adl_crates
    
## COMPILATION ##
Plug your device to your computer, then to compile the Ada project run: <br/>
`make PRJ_NAME=ecg_test`
You can replace 'ecg_test' with an other file in the src/test folder.<br/>
You should change in the makefile the port of your device if needed.

# TODO #
- [ ] Flash an ECG signal directly in the board. 

# ISSUES #
Python package wfdb and matplotlib wasn't working fine on linux. I had to create a
pyenv: <br/>
`python3 -m venv` <br/>
`source ecg_sensor/bin/activate`
Then, <br/>
`pip install wfdb matplotlib pystruct pyserial`
