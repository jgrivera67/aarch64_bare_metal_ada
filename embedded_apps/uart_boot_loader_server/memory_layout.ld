/**
 * Linker script for the UART boot loader server.
 *
 * NOTE: The boot loader image itself is assumed to be booted as a kernel8.img
 * from an SD card, so it will be loaded at address 0x80000.
 * The target boot image will be loaded at address 0x100000. So, the boot loader
 * can only use RAM below this address for its own code and internal data.
 */
ENTRY(reset_handler);

BOOTLOADER_RAM_SIZE = 512K;
BOOT_ADDRESS = 0x80000;
STACK_ALIGNMENT = 16;

MEMORY
{
  RAM(rwx) : ORIGIN = BOOT_ADDRESS, LENGTH = BOOTLOADER_RAM_SIZE
}

SECTIONS
{
 .text :
  {
    __global_text_region_start = .;
    *(.text.reset_handler)
    *(.text .text.*)
  } >RAM

  /*
   * ARM exception unwind table
   */
  .ARM.extab :
  {
    *(.ARM.extab* .gnu.linkonce.armextab.*)
  } >RAM

  /*
   * ARM exception unwind table index
   */
  .ARM.exidx :
  {
    PROVIDE_HIDDEN (__exidx_start = .);
    *(.ARM.exidx* .gnu.linkonce.armexidx.*)
    PROVIDE_HIDDEN (__exidx_end = .);
    __global_text_region_end = .;
  } >RAM

  .rodata :
  {
    __global_rodata_region_start = .;
    PROVIDE_HIDDEN (__rodata_start = .);
    *(.rodata .rodata.*)
    PROVIDE_HIDDEN (__rodata_end = .);
    __global_rodata_region_end = .;
    PROVIDE( __data_load = . );
  } >RAM

  .data :  AT (__data_load)
  {
    __global_data_region_start = .;
    PROVIDE( __data_start = . );
    *(.data .data.*)
    PROVIDE( __data_end = . );
  } >RAM

  .bss (NOLOAD)  :
  {
   PROVIDE( __bss_start = . );
    *(.bss .bss.*)
    *(COMMON)
    PROVIDE( __bss_end = . );
    __global_data_region_end = .;
  } >RAM

  .stack (NOLOAD)  :
  {
    . = ALIGN(STACK_ALIGNMENT);
    __stacks_start = .;
    *(.isr_stacks)
    __stacks_end = .;
  } >RAM
}
