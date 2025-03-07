/**
 * Linker script to create image to boot as kernel8.img from an SD card
 */
ENTRY(reset_handler);

TOTAL_RAM = 1024M;
BOOT_ADDRESS = 0x80000;
STACK_SIZE = 8K;
STACK_ALIGNMENT = 16;

MEMORY
{
  RAM(rwx) : ORIGIN =  BOOT_ADDRESS, LENGTH = TOTAL_RAM - BOOT_ADDRESS
}

SECTIONS
{
 .text :
  {
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
  } >RAM

  .rodata :
  {
    PROVIDE_HIDDEN (__rodata_start = .);
    *(.rodata .rodata.*)
    PROVIDE_HIDDEN (__rodata_end = .);
    PROVIDE( __data_load = . );
  } >RAM

  .data :  AT (__data_load)
  {
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
  } >RAM

  .stack (NOLOAD)  :
  {
    . = ALIGN(STACK_ALIGNMENT);
    _stack_start = .;
    . += STACK_SIZE;
    _stack_end = .;
  } >RAM
}
