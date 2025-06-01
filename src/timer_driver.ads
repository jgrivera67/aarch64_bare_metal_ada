
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary ARM generic timer driver
--
with CPU;
with System;
with Interfaces;

package Timer_Driver is

   procedure Initialize;

   type Timestamp_Us_Type is new Interfaces.Unsigned_64;

   function Get_Timestamp_Us return Timestamp_Us_Type
      with Pre => CPU.Cpu_In_Privileged_Mode;

   type Delta_Time_Us_Type is new Interfaces.Unsigned_32;

   type Timer_Interrupt_Callback_Pointer_Type is access procedure (Arg : System.Address);

   procedure Start_Timer (Timer_Interrupt_Callback_Pointer : Timer_Interrupt_Callback_Pointer_Type;
                          Timer_Interrupt_Callback_Arg : System.Address;
                          Expiration_Delta_Time_Us : Delta_Time_Us_Type)
      with Pre => Timer_Interrupt_Callback_Pointer /= null and then
                  Expiration_Delta_Time_Us /= 0 and then
                  CPU.Cpu_In_Privileged_Mode;

   procedure Stop_Timer
      with Pre => CPU.Cpu_In_Privileged_Mode;

private

   procedure Timer_Interrupt_Handler (Arg : System.Address)
      with Pre => CPU.Cpu_In_Privileged_Mode;

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
   --  Counter-timer Virtual Timer Control register
   --
   --  NOTE: We don't need to declare this register with Volatile_Full_Access,
   --  as it is not memory-mapped. It is accessed via MRS/MCR instructions.
   --
   type CNTV_CTL_Type is record
      ENABLE : Timer_Enable_Type := Timer_Disabled;
      IMASK : Timer_Interrupt_Mask_Type := Timer_Interrupt_Masked;
      ISTATUS : Timer_Status_Type := Timer_Condition_Not_Met;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First;

   for CNTV_CTL_Type use record
      ENABLE at 0 range 0 .. 0;
      IMASK at 0 range 1 .. 1;
      ISTATUS at 0 range 2 .. 2;
   end record;

   function Get_CNTV_CTL return CNTV_CTL_Type
      with Inline_Always;

   procedure Set_CNTV_CTL (CNTV_CTL_Value : CNTV_CTL_Type)
      with Inline_Always;

   --
   --  Counter-timer Virtual Timer TimerValue register
   --
   --  From section D23.10.27 of DDI0487K_a_a-profile_architecture_reference_manual.pdf:
   --   "On a read of this register:
   --  - If CNTV_CTL_EL0.ENABLE is 0, the value returned is UNKNOWN.
   --  - If CNTV_CTL_EL0.ENABLE is 1, the value returned is (CNTV_CVAL_EL0 - CNTVCT_EL0).
   --  On a write of this register, CNTV_CVAL_EL0 is set to (CNTVCT_EL0 + TimerValue), where
   --  TimerValue is treated as a signed 32-bit integer.
   --  When CNTV_CTL_EL0.ENABLE is 1, the timer condition is met when (CNTVCT_EL0 -
   --  CNTV_CVAL_EL0) is greater than or equal to zero. This means that TimerValue acts like a 32-bit
   --  downcounter timer. When the timer condition is met:
   --  • CNTV_CTL_EL0.ISTATUS is set to 1.
   --  • If CNTV_CTL_EL0.IMASK is 0, an interrupt is generated.
   --  When CNTV_CTL_EL0.ENABLE is 0, the timer condition is not met, but CNTVCT_EL0
   --  continues to count, so the TimerValue view appears to continue to count down."
   --
   --  NOTE: We don't need to declare this register with Volatile_Full_Access,
   --  as it is not memory-mapped. It is accessed via MRS/MSR instructions.
   --  Also, the value of this th register is treated as a signed 32-bit integer, it
   --  is a 64-bit register.
   --

   type Timer_Cycles_Type is new Interfaces.Unsigned_64;

   function Get_CNTV_TVAL return Timer_Cycles_Type
      with Inline_Always;

   procedure Set_CNTV_TVAL (CNTV_TVAL_Value : Timer_Cycles_Type)
      with Inline_Always;

   --
   --  Counter-timer Virtual Count register (Free-running counter)
   --
   function Get_CNTVCT return Timer_Cycles_Type
      with Inline_Always;

   function Get_CNTFRQ return Interfaces.Unsigned_64
      with Inline_Always;

   use type Interfaces.Unsigned_64;

   function Get_Timestamp_Cycles return Timer_Cycles_Type is
      (Get_CNTVCT);

   function Get_Timer_Counter_Cycles_Per_Usec return Timer_Cycles_Type is
      (Timer_Cycles_Type (Get_CNTFRQ / 1_000_000));

   function Get_Timestamp_Us return Timestamp_Us_Type is
      (Timestamp_Us_Type (Get_Timestamp_Cycles / Get_Timer_Counter_Cycles_Per_Usec));

   type Timer_Device_Type is limited record
      Interrupt_Callback_Pointer : Timer_Interrupt_Callback_Pointer_Type := null;
      Interrupt_Callback_Arg : System.Address := System.Null_Address;
      Last_Period_Time_Stamp_Cycles : Timer_Cycles_Type := 0;
      Timer_Interrupt_Small_Drift_Count : Interfaces.Unsigned_32 := 0;
      Timer_Interrupt_Big_Drift_Count : Interfaces.Unsigned_32 := 0;
   end record;

   Timer_Devices : array (CPU.Valid_Cpu_Core_Id_Type) of Timer_Device_Type;

end Timer_Driver;