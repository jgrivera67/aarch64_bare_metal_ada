
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary ARM generic timer driver
--
with System;
with Interfaces;

package Timer_Driver is

   procedure Initialize;

   function Get_Timestamp_Usec return Interfaces.Unsigned_64;

private

   ----------------------------------------------------------------------------
   --  ARMv8-A AArch64 Generic timer declarations
   ----------------------------------------------------------------------------

   type Timer_Enable_Type is (Timer_Disabled, Timer_Enabled)
      with Size => 1;

   for Timer_Enable_Type use
      (Timer_Disabled => 2#0#,
       Timer_Enabled => 2#1#);

   type Timer_Interrupt_Mask_Type is (Timer_Interrupt_Not_Masked,
                                      Timer_Interrupt_Masked)
      with Size => 1;

   for Timer_Interrupt_Mask_Type use
      (Timer_Interrupt_Not_Masked => 2#0#,
       Timer_Interrupt_Masked => 2#1#);

   type Timer_Status_Type is (Timer_Condition_Not_Met,
                              Timer_Condition_Met)
      with Size => 1;

   for Timer_Status_Type use
      (Timer_Condition_Not_Met => 2#0#,
       Timer_Condition_Met => 2#1#);

   --
   --  Counter-timer Physical Timer Control register
   --
   --  NOTE: We don't need to declare this register with Volatile_Full_Access,
   --  as it is not memory-mapped. It is accessed via MRS/MCR instructions.
   --
   type CNTP_CTL_Type is record
      ENABLE : Timer_Enable_Type := Timer_Disabled;
      IMASK : Timer_Interrupt_Mask_Type := Timer_Interrupt_Masked;
      ISTATUS : Timer_Status_Type := Timer_Condition_Not_Met;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First;

   for CNTP_CTL_Type use record
      ENABLE at 0 range 0 .. 0;
      IMASK at 0 range 1 .. 1;
      ISTATUS at 0 range 2 .. 2;
   end record;

   function Get_CNTP_CTL return CNTP_CTL_Type
      with Inline_Always;

   procedure Set_CNTP_CTL (CNTP_CTL_Value : CNTP_CTL_Type)
      with Inline_Always;

   function Get_CNTV_CTL return CNTP_CTL_Type
      with Inline_Always;

   procedure Set_CNTV_CTL (CNTP_CTL_Value : CNTP_CTL_Type)
      with Inline_Always;

   --
   --  Counter-timer Physical Timer TimerValue register
   --
   --  From section G8.7.18 of DDI0487Fc_armv8_arm.pdf:
   --  "On a read of this register:
   --  - If CNTP_CTL.ENABLE is 0, the value returned is UNKNOWN.
   --  - If CNTP_CTL.ENABLE is 1, the value returned is (CNTP_CVAL - CNTPCT).
   --
   --  On a write of this register, CNTP_CVAL is set to (CNTPCT + TimerValue), where
   --  TimerValue is treated as a signed 32-bit integer.
   --  When CNTP_CTL.ENABLE is 1, the timer condition is met when (CNTPCT - CNTP_CVAL)
   --  is greater than or equal to zero. This means that TimerValue acts like a 32-bit
   --  downcounter timer. When the timer condition is met:
   --  - CNTP_CTL.ISTATUS is set to 1.
   --  - If CNTP_CTL.IMASK is 0, an interrupt is generated.
   --
   --  When CNTP_CTL.ENABLE is 0, the timer condition is not met, but CNTPCT continues
   --  to count, so the TimerValue view appears to continue to count down."
   --
   --  NOTE: We don't need to declare this register with Volatile_Full_Access,
   --  as it is not memory-mapped. It is accessed via MRS/MSR instructions.
   --

   function Get_CNTP_TVAL return Interfaces.Unsigned_64
      with Inline_Always;

   procedure Set_CNTP_TVAL (CNTP_TVAL_Value : Interfaces.Unsigned_64)
      with Inline_Always;

   --
   --  Counter-timer Physical Count register (Free-running counter)
   --

   function Get_CNTPCT return Interfaces.Unsigned_64
      with Inline_Always;

   function Get_CNTVCT return Interfaces.Unsigned_64
      with Inline_Always;

   function Get_CNTFRQ return Interfaces.Unsigned_64
      with Inline_Always;

   use type Interfaces.Unsigned_64;

   function Get_Timestamp_Usec return Interfaces.Unsigned_64 is
         (Get_CNTVCT / (Get_CNTFRQ / 1_000_000));
end Timer_Driver;