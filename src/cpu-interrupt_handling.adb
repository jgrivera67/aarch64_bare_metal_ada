--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary HiRTOS to target platform interface for ARMv8-A aarch64 architecture - Interrupt handling
--

with CPU.Multicore;
with CPU.Self_Hosted_Debug;
with Utils.Runtime_Log;
with System.Machine_Code;

package body CPU.Interrupt_Handling is
   use ASCII;

   procedure Initialize is
   begin
      Interrupt_Controller_Driver.Initialize;
      Enable_Cpu_Interrupting;
   end Initialize;

   procedure Print_Exception_Info (Exception_Description : String) is
      use Utils.Runtime_Log;
      Cpu_Id : constant Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      ESR_EL1_Value : constant ESR_EL1_Type := Get_ESR_EL1;
      FAR_EL1_Value : constant Interfaces.Unsigned_64 := Get_FAR_EL1;
      ELR_EL1_Value : constant Interfaces.Unsigned_64 := Get_ELR_EL1;
   begin
      Log_Error_Msg_Begin ("*** EL1 ");
      Log_Error_Msg_Part (Exception_Description);
      Log_Error_Msg_Part (" (Exception class: ");
      Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (ESR_EL1_Value.EC'Enum_Rep));
      Log_Error_Msg_Part (", ESR_EL1: ");
      Log_Error_Value_Hexadecimal (ESR_EL1_Value.Value);
      Log_Error_Msg_Part (", FAR_EL1: ");
      Log_Error_Value_Hexadecimal (FAR_EL1_Value);
      Log_Error_Msg_Part (", faulting PC: ");
      Log_Error_Value_Hexadecimal (ELR_EL1_Value);

      --  Dump CPU registers:
      declare
         Interrupt_Nesting : constant Interrupt_Nesting_Type := Get_Cpu_Interrupt_Nesting (Cpu_Id);
         Stack_Pointer : constant System.Address :=
            Get_Interrupt_Nesting_Stack_Pointer (Interrupt_Nesting);
         Cpu_Context : constant Cpu_Context_Type with Import, Address => Stack_Pointer;
      begin
         pragma Assert (Cpu_Context.Registers (SP) = CPU.Cpu_Register_Type (To_Integer (Stack_Pointer)));
         Log_Error_Msg_Part (")" & LF & HT & "CPU Registers:" & LF & HT);
         for Cpu_Reg_Id in Cpu_Register_Id_Type loop
            Log_Error_Msg_Part (Cpu_Reg_Id'Image);
            Log_Error_Msg_Part (" = ");
            Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (
               Cpu_Context.Registers (Cpu_Reg_Id)));
            if Cpu_Reg_Id < Cpu_Register_Id_Type'Last then
               Log_Error_Msg_Part (LF & HT);
            end if;
         end loop;
      end;
      Log_Error_Msg_End;
   end Print_Exception_Info;

   procedure Handle_EL1_Error_Exception (Exception_Description : String) with No_Return is
   begin
      Print_Exception_Info (Exception_Description);
      CPU.Self_Hosted_Debug.Run_Debugger (CPU.Self_Hosted_Debug.Error_Exception_Event);
      Utils.System_Crash;
   end Handle_EL1_Error_Exception;

   procedure Handle_EL1_Debug_Exception (
      Debug_Event : CPU.Self_Hosted_Debug.Debug_Event_Type;
      Exception_Description : String) is
   begin
      Print_Exception_Info (Exception_Description);
      CPU.Self_Hosted_Debug.Run_Debugger (Debug_Event);
   end Handle_EL1_Debug_Exception;

   procedure Ada_Handle_EL1_Synchronous_Exception is
      ESR_EL1_Value : constant ESR_EL1_Type := Get_ESR_EL1;
   begin
      case ESR_EL1_Value.EC is
         when ESR_EL1_EC_Unknown =>
            Handle_EL1_Error_Exception ("Unknown Exception");
         when ESR_EL1_EC_Trapped_WFI_WFE =>
            Handle_EL1_Error_Exception ("Trapped WFI/WFE Exception");
         when ESR_EL1_EC_Trapped_Access_SME_SVE_Advanced_SIMD_FP =>
            Handle_EL1_Error_Exception ("Trapped Access SME/SVE/Advanced SIMD/FP Exception");
         when ESR_EL1_EC_Illegal_State =>
            Handle_EL1_Error_Exception ("Illegal State Exception");
         when ESR_EL1_EC_Trapped_MSR_MRS_System_Inst_In_AArch64 =>
            Handle_EL1_Error_Exception ("Trapped MSR/MRS/System Instruction in AArch64 Exception");
         when ESR_EL1_EC_Instruction_Abort_Lower_EL =>
            Handle_EL1_Error_Exception ("Instruction Abort at Lower Exception Level");
         when ESR_EL1_EC_Instruction_Abort_Current_EL =>
            Handle_EL1_Error_Exception ("Instruction Abort at Current Exception Level");
         when ESR_EL1_EC_PC_ALignment_Fault =>
            Handle_EL1_Error_Exception ("PC Alignment Fault Exception");
         when ESR_EL1_EC_Data_Abort_Lower_EL =>
            Handle_EL1_Error_Exception ("Data Abort at Lower Exception Level");
         when ESR_EL1_EC_Data_Abort_Current_EL =>
            Handle_EL1_Error_Exception ("Data Abort at Current Exception Level");
         when ESR_EL1_EC_SP_Alignment_Fault =>
            Handle_EL1_Error_Exception ("SP Alignment Fault Exception");
         when ESR_EL1_EC_Trapped_Floating_Point_Exception =>
            Handle_EL1_Error_Exception ("Trapped Floating Point Exception");
         when ESR_EL1_EC_Breakpoint_Lower_EL =>
            Handle_EL1_Debug_Exception (CPU.Self_Hosted_Debug.Hardware_Breakpoint_Event,
                                        "Hardware Breakpoint at Lower Exception Level");
         when ESR_EL1_EC_Breakpoint_Current_EL =>
            Handle_EL1_Debug_Exception (CPU.Self_Hosted_Debug.Hardware_Breakpoint_Event,
                                        "Hardware Breakpoint at Current Exception Level");
         when ESR_EL1_EC_Software_Step_Exception_Lower_EL =>
            Handle_EL1_Debug_Exception (CPU.Self_Hosted_Debug.Single_Step_Event,
                                        "Software Step Exception at Lower Exception Level");
         when ESR_EL1_EC_Software_Step_Exception_Current_EL =>
            Handle_EL1_Debug_Exception (CPU.Self_Hosted_Debug.Single_Step_Event,
                                        "Software Step Exception at Current Exception Level");
         when ESR_EL1_EC_Watchpoint_Lower_EL =>
            Handle_EL1_Debug_Exception (CPU.Self_Hosted_Debug.Watchpoint_Event,
                                        "Watchpoint at Lower Exception Level");
         when ESR_EL1_EC_Watchpoint_Current_EL =>
            Handle_EL1_Debug_Exception (CPU.Self_Hosted_Debug.Watchpoint_Event,
                                        "Watchpoint at Current Exception Level");
         when ESR_EL1_EC_BRK_Instruction_In_Aarch64 =>
            Handle_EL1_Debug_Exception (CPU.Self_Hosted_Debug.Software_Breakpoint_Event,
                                        "BRK Instruction in AArch64 Exception");
         when others =>
            Handle_EL1_Error_Exception ("Other Synchronous Error Exception");
      end case;
   end Ada_Handle_EL1_Synchronous_Exception;

   procedure Ada_Handle_EL1_Irq_Interrupt is
   begin
      Interrupt_Controller_Driver.GIC_Interrupt_Handler (
         Interrupt_Controller_Driver.Cpu_Interrupt_Irq);
   end Ada_Handle_EL1_Irq_Interrupt;

   procedure Ada_Handle_EL1_Fiq_Interrupt is
   begin
      Interrupt_Controller_Driver.GIC_Interrupt_Handler (
         Interrupt_Controller_Driver.Cpu_Interrupt_Fiq);
   end Ada_Handle_EL1_Fiq_Interrupt;

   procedure Ada_Handle_EL1_SError_Exception is
   begin
      Handle_EL1_Error_Exception ("SError Exception");
   end Ada_Handle_EL1_SError_Exception;

   procedure Ada_Handle_EL1_Unexpected_Exception is
   begin
      Handle_EL1_Error_Exception ("Unexpected Exception");
   end Ada_Handle_EL1_Unexpected_Exception;

   function Ada_Enter_Interrupt_Context (Stack_Pointer : System.Address) return System.Address is
      Cpu_Context : Cpu_Context_Type with Import, Address => Stack_Pointer;
      Cpu_Id : constant Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Interrupt_Nesting : Interrupt_Nesting_Type renames Cpu_To_Interrupt_Nesting (Cpu_Id);
   begin
      Cpu_Context.Registers (SP) := Cpu_Register_Type (To_Integer (Stack_Pointer));
      Increase_Interrupt_Nesting (Interrupt_Nesting, Stack_Pointer);
      return Stack_Pointer;
   end Ada_Enter_Interrupt_Context;

   function Ada_Exit_Interrupt_Context (Stack_Pointer : System.Address) return System.Address is
      Cpu_Context : Cpu_Context_Type with Import, Address => Stack_Pointer;
      Cpu_Id : constant Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Interrupt_Nesting : Interrupt_Nesting_Type renames Cpu_To_Interrupt_Nesting (Cpu_Id);
   begin
      pragma Assert (Cpu_Context.Registers (SP) = Cpu_Register_Type (To_Integer (Stack_Pointer)));
      Decrease_Interrupt_Nesting (Interrupt_Nesting);
      return Stack_Pointer;
   end Ada_Exit_Interrupt_Context;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm (
         "wfi",
         Volatile => True);
   end Wait_For_Interrupt;

   function Disable_Cpu_Interrupting return Cpu_Register_Type
   is
      DAIF_Value : constant DAIF_Type := Get_DAIF;
      PSTATE_Value : PSTATE_Type;
   begin
      if DAIF_Value.F = Interrupt_Enabled or else DAIF_Value.I = Interrupt_Enabled then
         System.Machine_Code.Asm (
            "msr DAIFset, %0",
            Inputs => Interfaces.Unsigned_8'Asm_Input ("g", DAIF_SetClr_IF_Mask),  --  %0
            Volatile => True);
      end if;

      Strong_Memory_Barrier;
      pragma Assert (Cpu_Interrupting_Disabled);
      PSTATE_Value.DAIF := DAIF_Value;
      return PSTATE_Value.Value;
   end Disable_Cpu_Interrupting;

   procedure Restore_Cpu_Interrupting (Old_Cpu_Interrupting : Cpu_Register_Type) is
      PSTATE_Value : constant PSTATE_Type := (As_Value => True, Value => Old_Cpu_Interrupting);
   begin
      Strong_Memory_Barrier;
      if PSTATE_Value.DAIF.I = Interrupt_Enabled and then PSTATE_Value.DAIF.F = Interrupt_Enabled then
         System.Machine_Code.Asm (
            "msr DAIFclr, %0",
            Inputs => Interfaces.Unsigned_8'Asm_Input ("g", DAIF_SetClr_IF_Mask),  --  %0
            Volatile => True);
      elsif PSTATE_Value.DAIF.I = Interrupt_Enabled then
         System.Machine_Code.Asm (
            "msr DAIFclr, %0",
            Inputs => Interfaces.Unsigned_8'Asm_Input ("g", DAIF_SetClr_I_Bit_Mask),  --  %0
            Volatile => True);
      elsif PSTATE_Value.DAIF.F = Interrupt_Enabled then
         System.Machine_Code.Asm (
            "msr DAIFclr, %0",
            Inputs => Interfaces.Unsigned_8'Asm_Input ("g", DAIF_SetClr_F_Bit_Mask),  --  %0
            Volatile => True);
      end if;
   end Restore_Cpu_Interrupting;

   procedure Enable_Cpu_Interrupting is
   begin
      System.Machine_Code.Asm (
         "dsb sy" & LF &
         "isb" & LF &
         "msr DAIFclr, %0",
         Inputs => Interfaces.Unsigned_8'Asm_Input ("g", DAIF_SetClr_IF_Mask),  --  %0
         Volatile => True);

      pragma Assert (not Cpu_Interrupting_Disabled);
   end Enable_Cpu_Interrupting;

   function Cpu_In_Interrupt_Context return Boolean is
      Cpu_Id : constant Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Interrupt_Nesting : Interrupt_Nesting_Type renames
        Cpu_To_Interrupt_Nesting (Cpu_Id);
   begin
      return Interrupt_Nesting.Nesting_Level > Interrupt_Nesting_Level_Type'First;
   end Cpu_In_Interrupt_Context;

   function Get_ESR_EL1 return ESR_EL1_Type is
      ESR_EL1_Value : ESR_EL1_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, esr_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", ESR_EL1_Value.Value), --  %0
         Volatile => True);

      return ESR_EL1_Value;
   end Get_ESR_EL1;

   function Get_FAR_EL1 return Interfaces.Unsigned_64 is
      FAR_EL1_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, far_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", FAR_EL1_Value), --  %0
         Volatile => True);

      return FAR_EL1_Value;
   end Get_FAR_EL1;

   function Get_ELR_EL1 return Interfaces.Unsigned_64 is
      ELR_EL1_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, elr_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", ELR_EL1_Value), --  %0
         Volatile => True);

      return ELR_EL1_Value;
   end Get_ELR_EL1;

   procedure Increase_Interrupt_Nesting
      (Interrupt_Nesting : in out Interrupt_Nesting_Type;
       Stack_Pointer : System.Address) is
   begin
      if Interrupt_Nesting.Nesting_Level = Interrupt_Nesting_Level_Type'Last then
         raise Program_Error with "Maximum Interrupt nesting exceeded";
      end if;

      Interrupt_Nesting.Nesting_Level := @ + 1;
      Interrupt_Nesting.Nesting_Level_To_Stack_Pointer_Map
         (Interrupt_Nesting.Nesting_Level) := Stack_Pointer;
   end Increase_Interrupt_Nesting;

   procedure Decrease_Interrupt_Nesting
      (Interrupt_Nesting : in out Interrupt_Nesting_Type) is
   begin
      if Interrupt_Nesting.Nesting_Level = Interrupt_Nesting_Level_Type'First then
         raise Program_Error with "Not running in interrupt context";
      end if;

      Interrupt_Nesting.Nesting_Level := @ - 1;
   end Decrease_Interrupt_Nesting;

   function Get_Saved_PC return System.Address is
      Interrupt_Nesting : Interrupt_Nesting_Type renames
        Cpu_To_Interrupt_Nesting (CPU.Multicore.Get_Cpu_Id);
      Saved_Stack_Pointer : constant System.Address :=
         Get_Interrupt_Nesting_Stack_Pointer (Interrupt_Nesting);
      Cpu_Context : constant Cpu_Context_Type with
         Import, Address => Saved_Stack_Pointer;
   begin
      return To_Address (Integer_Address (Cpu_Context.Registers (ELR_ELx_Or_PC)));
   end Get_Saved_PC;

   procedure Set_Saved_PC (PC_Value : System.Address) is
      Interrupt_Nesting : Interrupt_Nesting_Type renames
        Cpu_To_Interrupt_Nesting (CPU.Multicore.Get_Cpu_Id);
      Saved_Stack_Pointer : constant System.Address :=
         Get_Interrupt_Nesting_Stack_Pointer (Interrupt_Nesting);
      Cpu_Context : Cpu_Context_Type with
         Import, Address => Saved_Stack_Pointer;
   begin
      Cpu_Context.Registers (ELR_ELx_Or_PC) := Cpu_Register_Type (To_Integer (PC_Value));
   end Set_Saved_PC;

end CPU.Interrupt_Handling;
