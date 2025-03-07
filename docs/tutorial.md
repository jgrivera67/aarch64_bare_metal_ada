---
marp: false
title: Tutorial
author: J. German Rivera
paginate: true
---
# Developing Bare-metal Embedded Software in SPARK Ada for 64-bit ARM Platforms
**J. German Rivera**

josegrivera@tesla.com
---
---
<!--
This tutorial teaches how to develop 64-bit bare-metal embedded software for the
64-bit ARMv8-A (AArch64) architecture in SPARK Ada.
The Raspberry PI 4 (model B) board will be used as the target platform. The
tutorial will start by explaining how to build the GNAT
cross-toolchain for barematal AArch64 (aarch64-elf).
Then it will describe how to write your own UART boot loader for the Raspberry PI,
to make testing bare-metal code on the board easier.
Next, it will describe how to write a self-hosted bare-metal mini GDB server,
which can be used to set hardware break points and to examine stack traces and
memory from a GDB client over UART.
The second half of the tutorial will teach incrementally how to write a simple
64-bit multi-core RTOS, from the reset handler all the way to the thread scheduler,
explaining how to implement all the necessary building blocks in SPARK Ada along the
way.
This is a hands-on tutorial and to get the most out of it, attendees are encouraged
to bring their own RaspberryPI boards (4 model B or later), a microSD card already
formatted with Raspbian, a development laptop (preferably running Linux or
MacOS) with a microSD card reader, and a USB-to-serial cable to connect the
laptop to the Raspberry PI's UART0 serial port.
-->

## Topics
- [How to build the GNAT cross-tool chain from sources for bare-metal AArch64](#section_1)
- How to build a minimal platform-independent Ada Runtime library (RTS)
- Bare-metal Ada "Hello world" program: AArch64 startup code and minimal UART driver
- [Setting up Raspberry PI board for bare-metal development](#section_4)
- Writing a UART boot loader in Ada for RaspberryPI bare-metal programs
- AArch64 interrupt and exception handling in Ada
- Writing a self-hosted bare-metal mini GDB server in Ada
- Aarch64 GICv3 interrupt controller
- Aarch64 generic timer
- Memory protection using the AArch64 MMU
- Multi-core AArch64 startup code
- Designing a simple multicore RTOS
- Writing the RTOS thread scheduler in SPARK Ada
- Writing the RTOS thread synchronization primitives in SPARK Ada
---

<a id="section_1"></a>
## How to build the GNAT cross-tool chain from sources for bare-metal AArch64

### Download Sources
- Download latest gcc sources: https://www.gnu.org/software/gcc/
- Download latest binutils sources: https://www.gnu.org/software/binutils/

### Building gnat cross-compiler for bare-metal AArch64
Follow the steps described in https://wiki.osdev.org/GNAT_Cross-Compiler, making corresponding changes fro AArch64:

- Example for building on MacOS
Run script [build_aarch64_elf_gnat_toolchain_on_macos.sh](./third_party/build_aarch64_elf_gnat_toolchain_on_macos.sh)

- Example for building on Ubuntu Linux
Run script [build_aarch64_elf_gnat_toolchain_on_macos.sh](./third_party/build_aarch64_elf_gnat_toolchain_on_ubuntu.sh)

<a id="section_4"></a>
## Setting up Raspberry PI board for bare-metal development
- Install Raspberry Pi OS on a blank SD card using the Raspberry Pi Imager
  [tool](https://www.raspberrypi.com/software/)

- Enable UART console on the Raspberry PI board, by adding the following to the SD card
  `config.txt` file:
```
enable_uart=1
```