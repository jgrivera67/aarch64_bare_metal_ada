/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                  A R M                                   *
 *                                                                          *
 *                            Linker Script File                            *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2006 The European Space Agency            *
 *                   Copyright (C) 2003-2018 AdaCore                        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

ENTRY(reset_handler);

TOTAL_RAM = 1M;
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
