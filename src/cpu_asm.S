/**
 *  Copyright (c) 2025, German Rivera
 *
 *
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "cpu_asm.h"

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

    /*
     * Clear SCTLR_EL1:
     *
     * NOTE: Among other things, this ensures that the MMU is disabled, so that we
     * can execute instructions where the UART boot loader is located.
     */
    msr sctlr_el1, xzr

    /*
     * Jump back to UART boot loader:
     */
    mov x0, #KERNEL8_IMG_BOOT_ADDR
    br x0

    /*
     * Park the CPU
     */
1:  wfi
    b 1b

/**
 * MMU translation tables
 */
    .section .mmu_translation_tables
#if TRANSLATION_TABLES_SIZE_IN_BYTES % LEVEL1_TRANSLATION_RANGE_SIZE_IN_BYTES == 0
    .balign LEVEL1_TRANSLATION_RANGE_SIZE_IN_BYTES
#elif TRANSLATION_TABLES_SIZE_IN_BYTES % LEVEL2_TRANSLATION_RANGE_SIZE_IN_BYTES == 0
    .balign LEVEL2_TRANSLATION_RANGE_SIZE_IN_BYTES
#else
    .balign PAGE_SIZE_IN_BYTES
#endif
    .global mmu_translation_tables
mmu_translation_tables:
    .space TRANSLATION_TABLES_SIZE_IN_BYTES
