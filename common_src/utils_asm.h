/**
 *  Copyright (c) 2025, German Rivera
 *
 *
 *  SPDX-License-Identifier: Apache-2.0
 */

#pragma once

#define BIT(_bit_index) (1u << (_bit_index))

#define KERNEL8_IMG_BOOT_ADDR 0x80000u

#define UART_BOOT_LOAD_ADDR 0x100000u

#define MPIDR_EL1_CPU_ID_MASK 0x3u

#define DAIF_SETCLR_F_BIT_MASK BIT(0u)
#define DAIF_SETCLR_I_BIT_MASK BIT(1u)
#define DAIF_SETCLR_A_BIT_MASK BIT(2u)
#define DAIF_SETCLR_D_BIT_MASK BIT(3u)
#define DAIF_SETCLR_ALL_MASK (DAIF_SETCLR_D_BIT_MASK | DAIF_SETCLR_A_BIT_MASK | DAIF_SETCLR_I_BIT_MASK | DAIF_SETCLR_F_BIT_MASK)

.extern park_cpu
.extern _stack_end // defined in the linker script


