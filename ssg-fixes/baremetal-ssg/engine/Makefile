# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
#
# baremetal-ssg: RISC-V assembly static site generator
# Build with riscv64-linux-gnu toolchain

AS = riscv64-linux-gnu-as
LD = riscv64-linux-gnu-ld
OBJCOPY = riscv64-linux-gnu-objcopy

ASFLAGS = -march=rv64i -mabi=lp64
LDFLAGS = -static

SRC = src/baremetal.s
OBJ = build/baremetal.o
ELF = build/baremetal
BIN = build/baremetal.bin

.PHONY: all clean run qemu

all: $(ELF)

build:
	mkdir -p build

$(OBJ): $(SRC) | build
	$(AS) $(ASFLAGS) -o $@ $<

$(ELF): $(OBJ)
	$(LD) $(LDFLAGS) -o $@ $<

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

clean:
	rm -rf build _site

# Run in QEMU user-mode emulation
qemu: $(ELF)
	qemu-riscv64 $(ELF)

# Run natively (requires RISC-V hardware)
run: $(ELF)
	./$(ELF)
