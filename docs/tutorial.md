# Developing Bare-metal Embedded Software in SPARK Ada for 64-bit ARM Platforms
*J. German Rivera*<br>
jgrivera67@gmail.com

This tutorial teaches how to develop 64-bit bare-metal embedded software for the
64-bit ARMv8-A (AArch64) architecture in SPARK Ada.
The Raspberry PI 4 (model B) board will be used as the target platform. The
tutorial will start by explaining how to build the GNAT
cross-toolchain for bare-metal AArch64 (aarch64-elf).
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

## Topics
- [Setting up Raspberry PI board for bare-metal development](#section_1)
- [Building the GNAT cross-tool chain from sources for bare-metal AArch64](#section_2)
- [Building a minimal platform-independent Ada Runtime library (RTS)](#section_3)
- [AArch64 bare-metal Ada "Hello world" program](#section_4):
  - Raspberry PI boot sequence
  - AArch64 startup code
  - Minimal UART driver
- [Writing your own UART boot loader in Ada for RaspberryPI bare-metal programs](#section_5)
- [Writing your own bare-metal debug message logger in Ada](#section_6)
- [AArch64 bare-metal exception handling in Ada](#section_7)
- [Writing your own self-hosted bare-metal mini GDB server in Ada](#section_8)
- [Aarch64 bare-metal memory protection using the ARMv8-A MMU](#section_9)
- [AArch64 bare-metal interrupt handling in Ada](#section_10)
  - Arm GICv2 interrupt controller
  - Arm generic timer interrupts
  - UART Rx interrupts
- [AArch64 bare-metal multicore in Ada](#section_11)
  - Multi-core AArch64 startup code
  - Spinlocks
- [AArch64 bare-metal multicore interrupts in Ada](#section_12)

<a id="section_1"></a>
## Setting up Raspberry PI board for bare-metal development
- Install Raspberry Pi OS (64-bit) on a blank SD card using the Raspberry Pi Imager
  [tool](https://www.raspberrypi.com/software/)

### Preparing the Raspberry PI SD card

- Mount the formatted SD card on your development host
- Make sure 64-bit mode is enabled in `config.txt` on the SD card:
  ```
  # Run in 64-bit mode
  arm_64bit=1
  ```
- For Raspberry PI4, add the following to `config.txt` on the SD card:
  ```
  [all]
  enable_uart=1
  core_freq_min=500
  dtoverlay=disable-bt
  uart_2ndstage=1
  kernel=aarch64_hello_ada.bin
  #kernel=uart_boot_loader_server.bin
  ```
- For Raspberry PI5, add the following to `config.txt` on the SD card:
  ```
  [all]
  enable_uart=1
  dtparam=uart0=on
  uart_2ndstage=1
  kernel=aarch64_hello_ada.bin
  #kernel=uart_boot_loader_server.bin
  kernel_address=0x80000
  #armstub=bl31.bin
  ```

### Setting up development host for accessing the Raspberry PI debug UART
- Install the corresponding device driver for the serial to USB cable, for
  your development host. For example, for
  the PL2303TA USB to TTL Serial Cable, for MacOS, download and install [this driver](https://www.prolific.com.tw/us/showproduct.aspx?p_id=229&pcid=41).
  For Linux, The PL2303 driver is in the mainline Linux kernel.

<a id="section_2"></a>
## How to build the GNAT cross-tool chain from sources for bare-metal AArch64

### Download Sources
- Download latest binutils sources: https://www.gnu.org/software/binutils or https://ftp.gnu.org/gnu/binutils
- Download latest gcc sources: https://www.gnu.org/software/gcc or https://ftp.gnu.org/gnu/gcc
- Download latest gdb sources: https://www.gnu.org/software/gdb or https://ftp.gnu.org/gnu/gdb

## Building gnat cross-compiler for bare-metal AArch64
Follow the steps described in https://wiki.osdev.org/GNAT_Cross-Compiler, making corresponding changes for AArch64:

- Building on MacOS:
  Use script [build_aarch64_elf_gnat_toolchain_on_macos.sh](../third_party/build_aarch64_elf_gnat_toolchain_on_macos.sh).

  NOTE: If the build fails with the default native gcc compiler, try [gcc-14.2.0-3-aarch64-apple-darwin23](https://github.com/simonjwright/distributing-gcc/releases/download/gcc-14.2.0-3-aarch64/gcc-14.2.0-3-aarch64-apple-darwin23.pkg)

- Building on Ubuntu Linux:
Use script [build_aarch64_elf_gnat_toolchain_on_ubuntu.sh](../third_party/build_aarch64_elf_gnat_toolchain_on_ubuntu.sh)

A recent native gnat compiler is needed to build the cross-compiler. One way to get one is through
the `alr toolchain --select` command. This command installs gnat compilers at `~/.local/share/alire/toolchains/gnat_native_*

For both MacOS and Linux, update the following shell variables in the scripts mentioned above,
as needed, to match the versions of the toolchain sources to use:
- `binutils_version=2.44`
- `gcc_version=14.3.0`
- `gdb_version=16.3`

<a id="section_3"></a>
## Building a minimal platform-independent Ada Runtime library (RTS)

<a id="section_4"></a>
## AArch64 bare-metal Ada "Hello world" program

### Booting a 64-bit bare-metal program on the Raspberry PI

The sequence diagram below shows the boot sequence on the Raspberry PI 4:
![](uml_diagrams/raspberrypi4_boot_sequence.png)

The last boot stage loads a kernel image with the name `kernel8.img` by default,
from the SD card.
However, for bare-metal development, instead of `kernel8.img`, we will load our
own bare-metal binary image. For this, we need to add the following to the
`config.txt` file:

```
kernel=<bare-metal .bin file>
```

For example on MacOS, the SD card is mounted as `/Volumes/bootfs`. So,
we can copy our binary to the SD card as follows:
```
cp $bin_file /Volumes/bootfs
sync
```
<a id="section_5"></a>
## Writing your own UART boot loader in Ada for RaspberryPI bare-metal programs

<a id="section_6"></a>
## Writing your own bare-metal debug message logger in Ada

<a id="section_7"></a>
## AArch64 bare-metal exception handling in Ada

<a id="section_8"></a>
## Writing your own self-hosted bare-metal mini GDB server in Ada
- Sequence diagram ???
- Setting up ser2net

<a id="section_9"></a>
## Aarch64 bare-metal memory protection using the ARMv8-A MMU

<a id="section_10"></a>
## AArch64 bare-metal interrupt handling in Ada

<a id="section_11"></a>
AArch64 bare-metal multicore in Ada

<a id="section_12"></a>
- [AArch64 bare-metal multicore interrupts in Ada](#section_12)

![](memory_map.drawio.svg)