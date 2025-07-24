# 1. Introduction
This README explains how the code is design, what is does and usages of some packages for developpers.

# 2. Main application
The default compilation behaviour when using make builds the CLI application.  
This is a demonstration program where users can interact with the board 
using a terminal. The communication protocols and commands are described in the root folder. 

# 3. Channels 
This folder contain abstraction of peripherals as channels with Ada's polymorphism features, for 
a better code design.
It is not integrated in the Main application yet, as this one is currently using the package 
"virtual_adc" to handle channels.  

TODO: Continue doc for this chapter

# 4. CLI
This folder contains sources for storing parameters and a handler for commands and sensors.

## 4.1 Commands interpreter
This package store parameters of any types in a static argument pool. It provides
Accessor to register commands and read parameters during runtime. It uses dynamic allocation, as 
parameters can be of different type. This package does not handle UART communication, but it provides a procedure that takes a string as an argument, interprets it, and calls a callback when commands match those registered.. Please, refer to this [README](https://github.com/lgehu/ecg_sensor/blob/master/README.md) for further explanation of the command syntax.  
The 'cmd_test.adb' under the test folder provide an example of commands usage. 

## 4.2 Sensor handler
This package make the bridge between the commands line interpreter and virtual sensors.   
It is implemented in the main application.

TODO: Sensors should remove any references to the command interpreter from their source code to ensure independent code. The sensor handler could have a machine state where users can select the sensor they want. Specific commands/parameters will be registered by the sensor handler according to the selected sensor.