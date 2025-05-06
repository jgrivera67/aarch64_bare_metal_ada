/**
 * Linker script to create image to boot from the UART boot loader
 */
ENTRY(reset_handler);

TOTAL_RAM = 1024M;
BOOT_ADDRESS = 0x100000;
PAGE_ALIGNMENT = 4K;

MEMORY
{
  RAM(rwx) : ORIGIN =  BOOT_ADDRESS, LENGTH = TOTAL_RAM - BOOT_ADDRESS
}

SECTIONS
{
 .text :
  {
    . = ALIGN(PAGE_ALIGNMENT);
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
    . = ALIGN(PAGE_ALIGNMENT);
    __global_text_region_end = .;
  } >RAM

  .rodata :
  {
    . = ALIGN(PAGE_ALIGNMENT);
    __global_rodata_region_start = .;
    PROVIDE_HIDDEN (__rodata_start = .);
    *(.rodata .rodata.*)
    PROVIDE_HIDDEN (__rodata_end = .);
    . = ALIGN(PAGE_ALIGNMENT);
    __global_rodata_region_end = .;
    PROVIDE( __data_load = . );
  } >RAM

  .data :  AT (__data_load)
  {
    . = ALIGN(PAGE_ALIGNMENT);
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
    . = ALIGN(PAGE_ALIGNMENT);
    __global_data_region_end = .;
  } >RAM

  .stack (NOLOAD)  :
  {
    . = ALIGN(PAGE_ALIGNMENT);
    __stacks_start = .;
    *(.isr_stacks)
    . = ALIGN(PAGE_ALIGNMENT);
    __stacks_end = .;
  } >RAM

  .mmu_translation_tables (NOLOAD)  :
  {
    . = ALIGN(PAGE_ALIGNMENT);
    __mmu_translation_tables_start = .;
    *(.mmu_translation_tables)
    . = ALIGN(PAGE_ALIGNMENT);
    __mmu_translation_tables_end = .;
  } >RAM

}

ASSERT(__data_start == __data_load, "__data_start and __data_load must match");