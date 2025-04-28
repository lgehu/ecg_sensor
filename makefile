PRJ_NAME ?= ecg_sensor

# Toolchain
CC = arm-eabi-gcc
OBJCOPY = arm-eabi-objcopy
STFLASH = st-flash

# Build directory
BUILD_DIR = obj

# Default target
all: compile flash

# Compile the project
compile:
	alr build -- -XMAIN=$(PRJ_NAME) -largs obj/test.o
	alr exec -- $(OBJCOPY) -O binary $(BUILD_DIR)/$(PRJ_NAME) $(BUILD_DIR)/$(PRJ_NAME).bin

# Flash the binary to the board
flash:
	$(STFLASH) write $(BUILD_DIR)/$(PRJ_NAME).bin 0x8000000

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)/*
	alr exec -- gprclean

.PHONY: all compile flash clean