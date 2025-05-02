/*
 * Copyright (c) 2025, German Rivera
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include "cpu_asm.h"

/*
 * Imported symbols
 */
      .extern ada_handle_el1_synchronous_exception /* defined in interrupt_handling.adb */
      .extern ada_handle_el1_irq_interrupt         /* defined in interrupt_handling.adb */
      .extern ada_handle_el1_fiq_interrupt         /* defined in interrupt_handling.adb */
      .extern ada_handle_el1_serror_exception      /* defined in interrupt_handling.adb */
      .extern ada_handle_el1_unexpected_exception  /* defined in interrupt_handling.adb */

/**
 * ARMv8-A EL1 interrupt vector table
 * (see "Table D1-5 Vector offsets from vector table base address" in
 *  ARMv8 Architecture Reference Manual)
 */
      .section .text.interrupt_vector_table, "ax"
      .align 11 // 2^11-aligned (2K byte-aligned)
      .global interrupt_vector_table
interrupt_vector_table:
      // Vectors when taking exception/interrupt from same privileged ELx (x > 0) that was using SP EL0
      b same_el_with_sp_el0_synchronous_exception_handler // offset 0x0
      .align 7 // 2^7-aligned (128 byte-aligned)
      b same_el_with_sp_el0_irq_interrupt_handler         // offset 0x80
      .align 7 // 2^7-aligned (128 byte-aligned)
      b same_el_with_sp_el0_fiq_interrupt_handler         // offset 0x100
      .align 7 // 2^7-aligned (128 byte-aligned)
      b same_el_with_sp_el0_serror_exception_handler      // offset 0x180

      // Vectors when taking exception/interrupt from same privileged ELx (x > 0) that was using SP ELx
      .align 7 // 2^7-aligned (128 byte-aligned)
      b same_el_with_sp_elx_synchronous_exception_handler // offset 0x200
      .align 7 // 2^7-aligned (128 byte-aligned)
      b same_el_with_sp_elx_irq_interrupt_handler         // offset 0x280
      .align 7 // 2^7-aligned (128 byte-aligned)
      b same_el_with_sp_elx_fiq_interrupt_handler         // offset 0x300
      .align 7 // 2^7-aligned (128 byte-aligned)
      b same_el_with_sp_elx_serror_exception_handler      // offset 0x380

      // Vectors when taking exception/interrupt from lower ELx (x >= 0) that was running in aarch64 mode
      .align 7 // 2^7-aligned (128 byte-aligned)
      b lower_el_synchronous_exception_handler // offset 0x400
      .align 7 // 2^7-aligned (128 byte-aligned)
      b lower_el_irq_interrupt_handler         // offset 0x480
      .align 7 // 2^7-aligned (128 byte-aligned)
      b lower_el_fiq_interrupt_handler         // offset 0x500
      .align 7 // 2^7-aligned (128 byte-aligned)
      b lower_el_serror_exception_handler      // offset 0x580

      // Vectors when taking exception/interrupt from lower ELx (x >= 0) that was running in aarch32 mode
      .align 7 // 2^7-aligned (128 byte-aligned)
      b aarch32_lower_el_synchronous_exception_handler // offset 0x600
      .align 7 // 2^7-aligned (128 byte-aligned)
      b aarch32_lower_el_irq_interrupt_handler         // offset 0x680
      .align 7 // 2^7-aligned (128 byte-aligned)
      b aarch32_lower_el_fiq_interrupt_handler         // offset 0x700
      .align 7 // 2^7-aligned (128 byte-aligned)
      b aarch32_lower_el_serror_exception_handler      // offset 0x780

/**
 * Macro to be invoked at the beginning of top-level EL1 interrupt handlers from
 * which the RTOS scheduler can be called upon exit.
 *
 * It first switches the SPSel to SP_EL0, instead of corresponding exception
 * SP_ELx. Then it saves all general purpose registers on the stack. All registers
 * need to be saved (both caller-saved and callee-saved) because the task
 * resumed upon returning from the interrupt may be a different task. However,
 * we need to save all the registers only if the interrupt nesting level was 0
 * before this interrupt.
 *
 * @pre  interrupts are disabled at the CPU
 * @pre  CPU is in EL1, using SP_EL1 stack pointer
 * @post CPU is in EL1, using SP_EL0 stack pointer
 *
 * NOTE: We cannot check preconditions, as that would insert code
 * at the beginning of this subprogram, which would clobber the CPU registers
 * before we save them.
 */
      .macro INTERRUPT_HANDLER_PROLOG
      /*
       * Switch to use SP_EL0 to save ELR_EL1, SPSR_EL1 and general purpose registers
       * onto the interrupted context stack.
       *
       * NOTE: ELR_EL1 holds the exception return address and SPSR_EL1 is the
       * interrupted mode PSTATE.
       */
      msr SPSel, #0

      /*
       * Save general-purpose registers on the stack:
       *
       * NOTE: SP does not need to be saved here,
       * as it is saved in the interrupted task's TCB.
       */
      stp x0, x1, [sp, #-16]!
      stp x2, x3, [sp, #-16]!
      stp x4, x5, [sp, #-16]!
      stp x6, x7, [sp, #-16]!
      stp x8, x9, [sp, #-16]!
      stp x10, x11, [sp, #-16]!
      stp x12, x13, [sp, #-16]!
      stp x14, x15, [sp, #-16]!
      stp x16, x17, [sp, #-16]!
      stp x18, x19, [sp, #-16]!
      stp x20, x21, [sp, #-16]!
      stp x22, x23, [sp, #-16]!
      stp x24, x25, [sp, #-16]!
      stp x26, x27, [sp, #-16]!
      stp x28, x29, [sp, #-16]!
      stp x30, xzr, [sp, #-16]! // Keep stack 16-byte-aligned

      /*
       * Save ELR_EL1 and SPSR_EL1 on the stack:
       */
      mrs x0, elr_el1
      mrs x1, spsr_el1
      stp x0, x1, [sp, #-16]!

      /*
       * Call sp = Ada_Enter_Interrupt_Context (sp)
       */
      mov x0, sp
      bl ada_enter_interrupt_context
      mov sp, x0

      /*
       * NOTE: At this point sp always points to somewhere in the ISR stack
       *
       * Set frame pointer to be the same as stack pointer:
       * (needed for stack unwinding across interrupted contexts)
       */
      mov fp, sp
      .endm

/**
 * Macro to be invoked at the end of top-level EL1 interrupt
 * handlers from which the RTOS scheduler can be called upon exit.
 *
 * It restores the CPU state that was saved by a previous invocation to
 * Interrupt_Handler_Prolog.
 *
 * @pre  interrupts are disabled at the CPU
 * @pre  CPU is in EL1 mode and SP is SP_EL0
 * @post PC = return address from interrupt (next instruction to execute in
 *        interrupted code)
 * @post current CPU privilege = privilege level of interrupted code
 */
      .macro INTERRUPT_HANDLER_EPILOG
      /*
       * Call sp = HiRTOS.Interrupt_Handling.Exit_Interrupt_Context (sp)
       */
      mov x0, sp
      bl ada_exit_interrupt_context
      mov sp, x0

      /*
       * Restore ELR_EL1 and SPSR_EL1 on the stack:
       */
      ldp x0, x1, [sp], #16
      msr elr_el1, x0
      msr spsr_el1, x1

      /*
       * Restore general-purpose registers saved on the stack:
       */
      ldp x30, xzr, [sp], #16
      ldp x28, x29, [sp], #16
      ldp x26, x27, [sp], #16
      ldp x24, x25, [sp], #16
      ldp x22, x23, [sp], #16
      ldp x20, x21, [sp], #16
      ldp x18, x19, [sp], #16
      ldp x16, x17, [sp], #16
      ldp x14, x15, [sp], #16
      ldp x12, x13, [sp], #16
      ldp x10, x11, [sp], #16
      ldp x8, x9, [sp], #16
      ldp x6, x7, [sp], #16
      ldp x4, x5, [sp], #16
      ldp x2, x3, [sp], #16
      ldp x0, x1, [sp], #16

      /*
       * Return from EL1 exception:
       */
      eret
      .endm

/*
 * Entry point of the synchronous exception that fires at the current privileged ELx (x > 0)
 * that was using the SP EL0 stack pointer (thread context).
 *
 * NOTE: Right after entering an EL1 exception, sp is SP_EL1,
 * which we don't use, as we handle exceptions using SP_EL0.
 *
 * CAUTION: This routine cannot use any stack space, before it invokes
 * INTERRUPT_HANDLER_PROLOG, as we do not define a stack for SP_EL1.
 */
      .section .text.same_el_with_sp_el0_synchronous_exception_handler
      .align 2
      .global same_el_with_sp_el0_synchronous_exception_handler
      .func same_el_with_sp_el0_synchronous_exception_handler
same_el_with_sp_el0_synchronous_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_synchronous_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the IRQ interrupt that fires at the current privileged ELx (x > 0)
 * that was using the SP EL0 stack pointer (thread context).
 */
      .section .text.same_el_with_sp_el0_irq_interrupt_handler
      .align 2
      .global same_el_with_sp_el0_irq_interrupt_handler
      .func same_el_with_sp_el0_irq_interrupt_handler
   same_el_with_sp_el0_irq_interrupt_handler:
      // Save the current thread's CPU state on its own stack:
      INTERRUPT_HANDLER_PROLOG

      bl ada_handle_el1_irq_interrupt

      // Run the thread scheduler to select next thread to run and
      // resume execution of the newly selected thread
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the FIQ interrupt that fires at the current privileged ELx (x > 0)
 * that was using the SP EL0 stack pointer (thread context).
 */
      .section .text.same_el_with_sp_el0_fiq_interrupt_handler
      .align 2
      .global same_el_with_sp_el0_fiq_interrupt_handler
      .func same_el_with_sp_el0_fiq_interrupt_handler
   same_el_with_sp_el0_fiq_interrupt_handler:
      // Save the current thread's CPU state on its own stack:
      INTERRUPT_HANDLER_PROLOG

      bl ada_handle_el1_fiq_interrupt

      // Run the thread scheduler to select next thread to run and
      // resume execution of the newly selected thread
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the SError exception that fires at the current privileged ELx (x > 0)
 * that was using the SP EL0 stack pointer (thread context).
 */
      .section .text.same_el_with_sp_el0_serror_exception_handler
      .align 2
      .global same_el_with_sp_el0_serror_exception_handler
      .func same_el_with_sp_el0_serror_exception_handler
   same_el_with_sp_el0_serror_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_serror_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

      .section .text.same_el_with_sp_elx_synchronous_exception_handler
      .align 2
      .global same_el_with_sp_elx_synchronous_exception_handler
      .func same_el_with_sp_elx_synchronous_exception_handler
   same_el_with_sp_elx_synchronous_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the IRQ interrupt that fires at the current privileged ELx (x > 0)
 * that was using the SP ELx stack pointer (interrupt context).
 */
      .section .text.same_el_with_sp_elx_irq_interrupt_handler
      .align 2
      .global same_el_with_sp_elx_irq_interrupt_handler
      .func same_el_with_sp_elx_irq_interrupt_handler
   same_el_with_sp_elx_irq_interrupt_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the FIQ interrupt that fires at the current privileged ELx (x > 0)
 * that was using the SP ELx stack pointer (interrupt context).
 */
      .section .text.same_el_with_sp_elx_fiq_interrupt_handler
      .align 2
      .global same_el_with_sp_elx_fiq_interrupt_handler
      .func same_el_with_sp_elx_fiq_interrupt_handler
   same_el_with_sp_elx_fiq_interrupt_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the SError exception that fires at the current privileged ELx (x > 0)
 * that was using the SP ELx stack pointer (interrupt context).
 */
      .section .text.same_el_with_sp_elx_serror_exception_handler
      .align 2
      .global same_el_with_sp_elx_serror_exception_handler
      .func same_el_with_sp_elx_serror_exception_handler
   same_el_with_sp_elx_serror_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the synchronous exception that fires at a lower EL (lower privilege level)
 *
 * For SVC exceptions, register x0 indicates the action to perform:
 * - 0 perform RTOS task synchronous context switch
 * - 1 switch to privileged mode and return to the caller
 *
 * NOTE: Right after entering an EL1 exception, sp is SP_EL1,
 * which we don't use, as we handle exceptions using SP_EL0.
 *
 * CAUTION: This routine cannot use any stack space, before it invokes
 * INTERRUPT_HANDLER_PROLOG, as we do not define a stack for SP_EL1.
 */
      .section .text.lower_el_synchronous_exception_handler
      .align 2
      .global lower_el_synchronous_exception_handler
      .func lower_el_synchronous_exception_handler
   lower_el_synchronous_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_synchronous_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the IRQ interrupt that fires at a lower EL (lower privilege level)
 */
      .section .text.lower_el_irq_interrupt_handler
      .align 2
      .global lower_el_irq_interrupt_handler
      .func lower_el_irq_interrupt_handler
   lower_el_irq_interrupt_handler:
      // Save the current thread's CPU state on its own stack:
      INTERRUPT_HANDLER_PROLOG

      bl ada_handle_el1_irq_interrupt

      // Run the thread scheduler to select next thread to run and
      // resume execution of the newly selected thread
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the FIQ interrupt that fires at a lower EL (lower privilege level)
 */
      .section .text.lower_el_fiq_interrupt_handler
      .align 2
      .global lower_el_fiq_interrupt_handler
      .func lower_el_fiq_interrupt_handler
   lower_el_fiq_interrupt_handler:
      // Save the current thread's CPU state on its own stack:
      INTERRUPT_HANDLER_PROLOG

      bl ada_handle_el1_fiq_interrupt

      // Run the thread scheduler to select next thread to run and
      // resume execution of the newly selected thread
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the SError exception that fires at a lower EL (lower privilege level)
 */
      .section .text.lower_el_serror_exception_handler
      .align 2
      .global lower_el_serror_exception_handler
      .func lower_el_serror_exception_handler
   lower_el_serror_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_serror_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the synchronous exception that fires at a lower EL (lower privilege level),
 * when the CPU was in aarch32 execution state
 */
      .section .text.aarch32_lower_el_synchronous_exception_handler
      .align 2
      .global aarch32_lower_el_synchronous_exception_handler
      .func aarch32_lower_el_synchronous_exception_handler
   aarch32_lower_el_synchronous_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the IRQ interrupt that fires at a lower EL (lower privilege level)
 * when the CPU was in aarch32 execution state
 */
      .section .text.aarch32_lower_el_irq_interrupt_handler
      .align 2
      .global aarch32_lower_el_irq_interrupt_handler
      .func aarch32_lower_el_irq_interrupt_handler
   aarch32_lower_el_irq_interrupt_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the FIQ interrupt that fires at a lower EL (lower privilege level)
 * when the CPU was in aarch32 execution state
 */
      .section .text.aarch32_lower_el_fiq_interrupt_handler
      .align 2
      .global aarch32_lower_el_fiq_interrupt_handler
      .func aarch32_lower_el_fiq_interrupt_handler
   aarch32_lower_el_fiq_interrupt_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc

/*
 * Entry point of the SError exception that fires at a lower EL (lower privilege level)
 * when the CPU was in aarch32 execution state
 */
      .section .text.aarch32_lower_el_serror_exception_handler
      .align 2
      .global aarch32_lower_el_serror_exception_handler
      .func aarch32_lower_el_serror_exception_handler
   aarch32_lower_el_serror_exception_handler:
      INTERRUPT_HANDLER_PROLOG
      bl ada_handle_el1_unexpected_exception
      INTERRUPT_HANDLER_EPILOG
      .endfunc
