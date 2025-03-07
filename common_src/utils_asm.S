/**
 *  Copyright (c) 2025, German Rivera
 *
 *
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "utils_asm.h"

.section .text.park_cpu
.global park_cpu
park_cpu:
    /*
     * Disable all exceptions and interrupts:
     */
    msr DAIFset, #DAIF_SETCLR_ALL_MASK

    /*
     * If CPU core 0 and booted from UART, jump back to the UART boot loader:
     */
    mrs x0, mpidr_el1
    and x0, x0, #MPIDR_EL1_CPU_ID_MASK
    cbnz x0, 1f
    adr x0, reset_handler
    cmp x0, #UART_BOOT_LOAD_ADDR
    bne 1f
    mov x0, #KERNEL8_IMG_BOOT_ADDR
    br x0

    /*
     * Park the CPU
     */
1:  wfi
    b 1b
