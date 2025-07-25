# 1. Introduction
This README explains how the code is design, what is does and usages of some packages.

# 2. Main application
The default compilation behaviour when using make builds the CLI application.  
This is a demonstration program where users can interact with the board 
using a terminal. The communication protocols and commands are described in the root folder. 

# 3. Channels 
This folder contain abstraction of peripherals as channels with Ada's polymorphism features, for 
a better code design. Plus, channels as their own circular buffer, that can lead to multichannel sampling in the same time.
It is not integrated in the Main application yet, as this one is currently using the package 
"virtual_adc" to handle channels.  
Channels usages can be found under the test folder ([channel_adc_test](test/channel_adc_test.adb) and [channel_memory_test](test/channel_memory_test.adb))

TODO: Continue doc for this chapter

# 4. CLI
This folder contains sources for storing parameters and a handler for commands and sensors.

## 4.1 Commands interpreter
This package store parameters of any types in a static argument pool. It provides
Accessor to register commands and read parameters during runtime. It uses dynamic allocation, as 
parameters can be of different type. This package does not handle UART communication, but it provides a procedure that takes a string as an argument, interprets it, and calls a callback when commands match those registered.. Please, refer to this [README](../README.md) for further explanation of the command syntax.  
The 'cmd_test.adb' under the test folder provide an example of commands usage. 

## 4.2 Sensor handler
This package make the bridge between the commands line interpreter and virtual sensors.   
It is implemented in the main application.

TODO: Sensors should remove any references to the command interpreter from their source code to ensure independent code. The sensor handler could have a machine state where users can select the sensor they want. Specific commands/parameters will be registered by the sensor handler according to the selected sensor.

# 5. Config
This folder contains configuration for peripherals, GPIO, timers and so one. It is mainly used in
the sensor handler. The peripherals can be modified to be adapted on differents cards or application purposes.  

# 6. Controllers
Packages in this folder handle peripherials for specific tasks. For example, the "sampler" 
uses a timer to sample at a given frequency. The "led_controller" blink a LED N times at a given frequency.  
The virtual_adc, as mentioned in the chapter 3, is the old way to handle channels for sampling.
Using a state machine, it swap between different peripherals, thus the code can be messy and should be replaced with channels abstraction. 

TODO: Remove "with Peripherals" in the [virtual_adc](controllers/virtual_adc.ads) and the [sampler](controllers/sampler.ads), instead add discriminant to configure peripherals.

# 7. Sensors
Sensors are part of the main application and are design to be working with the sensor_handler. 
There is currently two sensors, ECG and speech detector.   
The ECG sensor uses the Pantompkins algorithm to detect peaks and calculate the heart rate.  
The speech detector detect if voice is detected in an audio source.
Users can attach callback to sensor to be notified when an event occured, a peak for the ecg or 
a voice for the speech detector.
The [full_demo.adb] is a pipeline demonstration that uses channel, sampler and a sensor. 

# 8. Tests
Test can be compiled with the following command:   
`make MAIN=full_demo`   
You can replace 'full_demo' by any others files in the folder without extension.
