/**
 *  Copyright (c) 2025, German Rivera
 *
 *
 *  SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#define BIT(_bit_index) (1u << (_bit_index))

#define MULTI_BIT_MASK(_most_significant_bit_index,                     \
                       _least_significant_bit_index)                    \
        (BIT(_most_significant_bit_index) |                             \
         ((BIT(_most_significant_bit_index) - 1u) &                     \
          ~(BIT(_least_significant_bit_index) - 1u)))

#if 0 // ???
/*
 * NOTE: This address must match the value of BOOT_ADDRESS in sd_card_boot_memory_layout.ld
 */
#define DEFAULT_BOOT_ADDR     0x80000u

/*
 * NOTE: This address must match the value of BOOT_ADDRESS in uart_boot_memory_layout.ld
 */
#define UART_BOOT_LOAD_ADDR   0x100000u
#else
/*
 * NOTE: This address must match the value of BOOT_ADDRESS in sd_card_boot_memory_layout.ld
 */
#define DEFAULT_BOOT_ADDR     0x80000u

/*
 * NOTE: This address must match the value of BOOT_ADDRESS in uart_boot_memory_layout.ld
 */
#define UART_BOOT_LOAD_ADDR   0x100000u
#endif

/*
 * MPIDR_EL1 bit fields
 */
#define MPIDR_EL1_AFF0_LSB 0u
#define MPIDR_EL1_AFF0_WIDTH 8u
#define MPIDR_EL1_AFF1_LSB 8u
#define MPIDR_EL1_AFF1_WIDTH 8u
#define MPIDR_EL1_AFF2_LSB 16u
#define MPIDR_EL1_AFF2_WIDTH 8u
#define MPIDR_EL1_MT_MASK BIT(24u)

/*
 * Bit masks to use with msr DAIFset/DAIFclr:
 */
#define DAIF_SETCLR_F_BIT_MASK BIT(0u)
#define DAIF_SETCLR_I_BIT_MASK BIT(1u)
#define DAIF_SETCLR_A_BIT_MASK BIT(2u)
#define DAIF_SETCLR_D_BIT_MASK BIT(3u)
#define DAIF_SETCLR_ALL_MASK (DAIF_SETCLR_D_BIT_MASK | DAIF_SETCLR_A_BIT_MASK | DAIF_SETCLR_I_BIT_MASK | DAIF_SETCLR_F_BIT_MASK)

/*
 * Bit masks for HCR_EL2 register bit fields
 */
#define HCR_EL2_RW_MASK BIT(31u)

/*
 * Bit masks PSTATE register bit fields
 */
#define PSTATE_SPSEL_MASK BIT(0) // Stack pointer selector: 1 = SP_ELx (x > 0), 0 = SP_EL0
#define PSTATE_EL_MASK  MULTI_BIT_MASK(3u, PSTATE_EL_SHIFT)
#define PSTATE_EL_SHIFT 2u
#define PSTATE_F_BIT_MASK BIT(6u) // Fiq
#define PSTATE_I_BIT_MASK BIT(7u) // Irq
#define PSTATE_A_BIT_MASK BIT(8u) // SError (Async Abort)
#define PSTATE_D_BIT_MASK BIT(9u) // Debug
#define PSTATE_DAIF_MASK (PSTATE_D_BIT_MASK | PSTATE_A_BIT_MASK | PSTATE_I_BIT_MASK | PSTATE_F_BIT_MASK)

/*
 * Bit masks for PSTATE EL field values:
 */
#define PSTATE_EL_EL0_MASK (0x0 << PSTATE_EL_SHIFT)
#define PSTATE_EL_EL1_MASK (0x1 << PSTATE_EL_SHIFT)
#define PSTATE_EL_EL2_MASK (0x2 << PSTATE_EL_SHIFT)
#define PSTATE_EL_EL3_MASK (0x3 << PSTATE_EL_SHIFT)

/*
 * Bit masks for ESR_EL1 register bit fields
 */
#define ESR_EL1_SVC_ISS_MASK MULTI_BIT_MASK(24u, ESR_EL1_SVC_ISS_SHIFT)
#define ESR_EL1_SVC_ISS_SHIFT 0u
#define ESR_EL1_EC_MASK MULTI_BIT_MASK(31u, ESR_EL1_EC_SHIFT)
#define ESR_EL1_EC_SHIFT 26u
#define ESR_EL1_EC_AARCH64_SVC_EXCEPTION 0x15u

/*
 * Bit masks for CPUECTLR_EL1 register bit fields
 */
#define CPUECTLR_EL1_SMPEN_MASK BIT(6u)

/*
 * Bit masks for SCTLR_EL1 register bit fields
 */
#define SCTLR_EL1_A_MASK BIT(1u)

#define GUARDED_ISR_STACK_SIZE_IN_BYTES (ISR_STACK_SIZE_IN_BYTES + PAGE_SIZE_IN_BYTES)

/*
 * NOTE: The value of this constant must match the value of `ISR_Stack_Size_In_Bytes`
 * in cpu-interrupt_handling.ads
 */
#define ISR_STACK_SIZE_IN_BYTES (4u * PAGE_SIZE_IN_BYTES)

#define LEVEL1_TRANSLATION_RANGE_SIZE_IN_BYTES (1024u * 1024u * 1024u) // 1Gb
#define LEVEL2_TRANSLATION_RANGE_SIZE_IN_BYTES (2u * 1024u * 1024u) // 2Mb
#define PAGE_SIZE_IN_BYTES (4u * 1024u) // 4Kb

#define PER_CPU_TRANSLATION_TABLES_SIZE_IN_BYTES \
        (MAX_NUM_TRANSLATION_TABLES_PER_CPU * PAGE_SIZE_IN_BYTES)

#define TRANSLATION_TABLES_SIZE_IN_BYTES \
        (NUM_CPUS * PER_CPU_TRANSLATION_TABLES_SIZE_IN_BYTES)

#define NUM_CPUS 4u

#define CACHE_LINE_SIZE_IN_BYTES  64u

/*
 * NOTE: The value of this constant must match `Max_Num_Translation_Tables_Per_Cpu`
 * in cpu-memory_protection.ads
 */
#define MAX_NUM_TRANSLATION_TABLES_PER_CPU 1024u

        .extern interrupt_vector_table
        .extern isr_stacks
        .extern park_cpu

/**
 * Get CPU id from MPIDR
 * @post CPU id is in \_dest_reg_
 * Clobbered registers: x1
 */
        .macro GET_CPU_ID _dest_reg_
        mrs \_dest_reg_, mpidr_el1
        and x1, \_dest_reg_, #MPIDR_EL1_MT_MASK
        cbnz x1, 0f
        ubfx \_dest_reg_, \_dest_reg_, #MPIDR_EL1_AFF0_LSB, #MPIDR_EL1_AFF0_WIDTH
        b 1f
0:
        ubfx \_dest_reg_, \_dest_reg_, #MPIDR_EL1_AFF1_LSB, #MPIDR_EL1_AFF1_WIDTH
1:
        .endm