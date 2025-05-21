MAIN ?= ecg_sensor
#PRJ_NAME ?= ecg_sensor

# Toolchain
CC = arm-eabi-gcc
OBJCOPY = arm-eabi-objcopy
STFLASH = st-flash

# Build directory
BUILD_DIR = obj
BIN_DIR = bin

# Default target
all: compile flash

# Compile the project
compile:
	alr build -- -XMAIN=$(MAIN) -v
	alr exec -- $(OBJCOPY) -O binary $(BIN_DIR)/$(MAIN) $(BIN_DIR)/$(MAIN).bin

# Flash the binary to the board
flash:
	$(STFLASH) write $(BIN_DIR)/$(MAIN).bin 0x8000000

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)/*
	rm -rf $(BIN_DIR)/*
	alr exec -- gprclean

.PHONY: all compile flash clean
