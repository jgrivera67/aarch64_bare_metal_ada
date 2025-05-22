--
--  Copyright (c) 2022-2023, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary ARM generic timer driver
--

with CPU.Interrupt_Handling;
with Interrupt_Controller_Driver;
with System.Machine_Code;
with System.Storage_Elements;

package body Timer_Driver is
   use System.Storage_Elements;

   procedure Initialize is
      CNTV_CTL_Value : CNTV_CTL_Type;
   begin
      CNTV_CTL_Value := Get_CNTV_CTL;
      CNTV_CTL_Value.ENABLE := Timer_Disabled;
      CNTV_CTL_Value.IMASK := Timer_Interrupt_Masked;
      Set_CNTV_CTL (CNTV_CTL_Value);

      --
      --  NOTE: the generic timer interrupt is enabled in the GIC and in the
      --  peripheral when Start_Timer is called and disabled when Stop_Timer
      --  is called.
      --
   end Initialize;

   procedure Start_Timer (Timer_Interrupt_Callback_Pointer : Timer_Interrupt_Callback_Pointer_Type;
                          Timer_Interrupt_Callback_Arg : System.Address;
                          Expiration_Delta_Time_Us : Delta_Time_Us_Type) is
      CNTV_CTL_Value : CNTV_CTL_Type;
      CNTV_TVAL_Value : constant Interfaces.Unsigned_64 :=
         Interfaces.Unsigned_64 (Expiration_Delta_Time_Us) *
         Get_Timer_Counter_Cycles_Per_Usec;
      Timer_Interrupt_Id : Interrupt_Controller_Driver.Valid_Interrupt_Id_Type renames
         CPU.Interrupt_Handling.Generic_Virtual_Timer_Interrupt_Id;
   begin
      pragma Assert (CNTV_TVAL_Value >= Interfaces.Unsigned_64 (Expiration_Delta_Time_Us));

      Timer_Device.Last_Period_Time_Stamp_Cycles := Get_Timestamp_Cycles;
      Timer_Device.Interrupt_Callback_Pointer := Timer_Interrupt_Callback_Pointer;
      Timer_Device.Interrupt_Callback_Arg := Timer_Interrupt_Callback_Arg;

      --
      --  Enable tick timer interrupt in the generic timer peripheral:
      --
      --  NOTE: Section G8.7.16 of ARMv8 Architecture Reference Manual says:
      --  "When the value of the ENABLE bit is 1, ISTATUS indicates whether
      --   the timer condition is met. ISTATUS takes no account of the value
      --   of the IMASK bit. If the value of ISTATUS is 1 and the value of
      --   IMASK is 0 then the timer interrupt is asserted.
      --   Setting the ENABLE bit to 0 disables the timer output signal,
      --   but the timer value accessible from CNTV_TVAL continues
      --   to count down. Disabling the output signal might be a power-saving
      --   option."
      --
      Set_CNTV_TVAL (CNTV_TVAL_Value);
      CNTV_CTL_Value := Get_CNTV_CTL;
      CNTV_CTL_Value.ENABLE := Timer_Enabled;
      CNTV_CTL_Value.IMASK := Timer_Interrupt_Not_Masked;
      Set_CNTV_CTL (CNTV_CTL_Value);

      --  Configure generic virtual timer interrupt in the GIC:
      Interrupt_Controller_Driver.Configure_Internal_Interrupt (
         Internal_Interrupt_Id => Timer_Interrupt_Id,
         Priority => CPU.Interrupt_Handling.Interrupt_Priorities (Timer_Interrupt_Id),
         Cpu_Interrupt_Line => Interrupt_Controller_Driver.Cpu_Interrupt_Irq,
         Trigger_Mode => Interrupt_Controller_Driver.Interrupt_Level_Sensitive,
         Interrupt_Handler_Entry_Point => Timer_Interrupt_Handler'Access,
         Interrupt_Handler_Arg => To_Address (Integer_Address (CNTV_TVAL_Value)));

      --  Enable generic timer interrupt in the GIC:
      Interrupt_Controller_Driver.Enable_Internal_Interrupt (Timer_Interrupt_Id);
   end Start_Timer;

   procedure Stop_Timer is
      CNTV_CTL_Value : CNTV_CTL_Type;
   begin
      --  Disable generic timer interrupt in the GIC:
      Interrupt_Controller_Driver.Disable_Internal_Interrupt (
         CPU.Interrupt_Handling.Generic_Virtual_Timer_Interrupt_Id);

      --  Disable tick timer interrupt in the generic timer peipheral:
      CNTV_CTL_Value := Get_CNTV_CTL;
      CNTV_CTL_Value.ENABLE := Timer_Disabled;
      CNTV_CTL_Value.IMASK := Timer_Interrupt_Masked;
      Set_CNTV_CTL (CNTV_CTL_Value);
   end Stop_Timer;

   procedure Timer_Interrupt_Handler (Arg : System.Address)
   is
      use type Interfaces.Unsigned_32;
      Timer_Period_Cycles : constant Interfaces.Unsigned_64 :=
         Interfaces.Unsigned_64 (To_Integer (Arg));
      Current_Time_Stamp_Cycles : constant Interfaces.Unsigned_64 := Get_Timestamp_Cycles;
      Expected_Period_Time_Stamp_Cycles : constant Interfaces.Unsigned_64 :=
         Timer_Device.Last_Period_Time_Stamp_Cycles + Timer_Period_Cycles;
      Time_Delta_Cycles : constant Interfaces.Unsigned_64 :=
         Current_Time_Stamp_Cycles - Expected_Period_Time_Stamp_Cycles;
   begin
      Timer_Device.Interrupt_Callback_Pointer (Timer_Device.Interrupt_Callback_Arg);
      declare
         Timer_Period_Drift_Cycles : constant Interfaces.Unsigned_64 :=
            (if Time_Delta_Cycles <= Interfaces.Unsigned_64 (Interfaces.Unsigned_32'Last) then
               Time_Delta_Cycles
             else
               Timer_Period_Cycles);
      begin
         --
         --  NOTE: Setting CNTV_TVAL here serves two purposes:
         --  - Clear the timer interrupt at the timer peripheral
         --  - Set the timer to fire for the next tick
         --
         if Timer_Period_Drift_Cycles = 0 then
            Timer_Device.Last_Period_Time_Stamp_Cycles := Expected_Period_Time_Stamp_Cycles;
            Set_CNTV_TVAL (Timer_Period_Cycles);
         elsif Timer_Period_Drift_Cycles < Timer_Period_Cycles then
            Timer_Device.Timer_Interrupt_Small_Drift_Count := @ + 1;
            Timer_Device.Last_Period_Time_Stamp_Cycles := Expected_Period_Time_Stamp_Cycles;
            Set_CNTV_TVAL (Timer_Period_Cycles - Timer_Period_Drift_Cycles);
         else
            Timer_Device.Timer_Interrupt_Big_Drift_Count := @ + 1;
            Timer_Device.Last_Period_Time_Stamp_Cycles := Current_Time_Stamp_Cycles;
            Set_CNTV_TVAL (Timer_Period_Cycles);
         end if;
      end;
   end Timer_Interrupt_Handler;

   function Get_CNTV_CTL return CNTV_CTL_Type is
      CNTV_CTL_Value : CNTV_CTL_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntv_ctl_el0",
         Outputs => CNTV_CTL_Type'Asm_Output ("=r", CNTV_CTL_Value), --  %0
         Volatile => True);

      return CNTV_CTL_Value;
   end Get_CNTV_CTL;

   procedure Set_CNTV_CTL (CNTV_CTL_Value : CNTV_CTL_Type) is
   begin
      System.Machine_Code.Asm (
         "msr cntv_ctl_el0, %0",
         Inputs => CNTV_CTL_Type'Asm_Input ("r", CNTV_CTL_Value), --  %0
         Volatile => True);
   end Set_CNTV_CTL;

   function Get_CNTV_TVAL return Interfaces.Unsigned_64 is
      CNTV_TVAL_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntv_tval_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTV_TVAL_Value), --  %0
         Volatile => True);

      return CNTV_TVAL_Value;
   end Get_CNTV_TVAL;

   procedure Set_CNTV_TVAL (CNTV_TVAL_Value : Interfaces.Unsigned_64) is
   begin
      System.Machine_Code.Asm (
         "msr cntv_tval_el0, %0",
         Inputs => Interfaces.Unsigned_64'Asm_Input ("r", CNTV_TVAL_Value), --  %0
         Volatile => True);
   end Set_CNTV_TVAL;

   function Get_CNTVCT return Interfaces.Unsigned_64 is
      CNTVCT_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntvct_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTVCT_Value), --  %0
         Volatile => True);

      return CNTVCT_Value;
   end Get_CNTVCT;

   function Get_CNTFRQ return Interfaces.Unsigned_64 is
      CNTFRQ_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntfrq_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTFRQ_Value), --  %0
         Volatile => True);

      return CNTFRQ_Value;
   end Get_CNTFRQ;

end Timer_Driver;