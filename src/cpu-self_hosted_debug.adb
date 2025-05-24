--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with CPU.Interrupt_Handling;
with Gdb_Server;
with System.Machine_Code;

package body CPU.Self_Hosted_Debug is

   procedure Run_Debugger (Debug_Event : Debug_Event_Type)
   is
      use type System.Address;
      Saved_PC : constant System.Address := CPU.Interrupt_Handling.Get_Saved_PC;
      Current_PC : System.Address := Saved_PC;
   begin
      Gdb_Server.Run_Gdb_Server (Debug_Event, Current_PC);
      if Current_PC /= Saved_PC then
         CPU.Interrupt_Handling.Set_Saved_PC (Current_PC);
      else
         CPU.Interrupt_Handling.Set_Saved_PC (
            To_Address (To_Integer (Saved_PC) + CPU.Instruction_Size_In_Bytes));
      end if;
   end Run_Debugger;

   function Get_Self_Hosted_Debug_Capabilities return Self_Hosted_Debug_Capabilities_Type
   is
      ID_AA64DFR0_EL1_Value : constant ID_AA64DFR0_EL1_Type := Get_ID_AA64DFR0_EL1;
   begin
      return (Num_Hardware_Breakpoints =>
                 Num_Hardware_Breakpoints_Type (ID_AA64DFR0_EL1_Value.BRPs) + 1,
              Num_Watchpoints =>
                 Num_Watchpoints_Type (ID_AA64DFR0_EL1_Value.WRPs) + 1);
   end Get_Self_Hosted_Debug_Capabilities;

   procedure Enable_Self_Hosted_Debugging is
      OSLAR_EL1_Value : OSLAR_EL1_Type;
      OSLSR_EL1_Value : OSLSR_EL1_Type;
      MDSCR_EL1_Value : MDSCR_EL1_Type;
   begin
      --  Disable OS lock:
      OSLAR_EL1_Value.OSLK := 0;
      Set_OSLAR_EL1 (OSLAR_EL1_Value);
      loop
         OSLSR_EL1_Value := Get_OSLSR_EL1;
         exit when OSLSR_EL1_Value.OSLK = 0;
      end loop;

      --
      --  Enable generation of HW Breakpoint, Watchpoint, and Vector Catch exceptions:
      --
      MDSCR_EL1_Value := Get_MDSCR_EL1;
      MDSCR_EL1_Value.MDE := 1;
      MDSCR_EL1_Value.KDE := 1;
      Set_MDSCR_EL1 (MDSCR_EL1_Value);

      --
      --  Enable self-hosted debug exceptions at the CPU:
      --
      System.Machine_Code.Asm (
         "msr DAIFclr, %0",
         Inputs => Interfaces.Unsigned_8'Asm_Input ("g", DAIF_SetClr_D_Bit_Mask),  --  %0
         Volatile => True);
   end Enable_Self_Hosted_Debugging;

   procedure Disable_Self_Hosted_Debugging is
      OSLAR_EL1_Value : OSLAR_EL1_Type;
      OSLSR_EL1_Value : OSLSR_EL1_Type;
      MDSCR_EL1_Value : MDSCR_EL1_Type;
   begin
      --
      --  Disable self-hosted debug exceptions at the CPU:
      --
      System.Machine_Code.Asm (
         "msr DAIFset, %0",
         Inputs => Interfaces.Unsigned_8'Asm_Input ("g", DAIF_SetClr_D_Bit_Mask),  --  %0
         Volatile => True);

      --
      --  Disable generation of HW Breakpoint, Watchpoint, and Vector Catch exceptions:
      --
      MDSCR_EL1_Value := Get_MDSCR_EL1;
      MDSCR_EL1_Value.MDE := 0;
      MDSCR_EL1_Value.KDE := 0;
      Set_MDSCR_EL1 (MDSCR_EL1_Value);

      --  Enable OS lock:
      OSLAR_EL1_Value.OSLK := 1;
      Set_OSLAR_EL1 (OSLAR_EL1_Value);
      loop
         OSLSR_EL1_Value := Get_OSLSR_EL1;
         exit when OSLSR_EL1_Value.OSLK = 1;
      end loop;
   end Disable_Self_Hosted_Debugging;

   procedure Set_Hardware_Breakpoint (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type;
                                      Code_Address : System.Address) is
      DBGBVRx_EL1_Value : DBGBVRx_EL1_Type;
      DBGBCRx_EL1_Value : DBGBCRx_EL1_Type;
   begin
      --  First disable breakpoint to be safe
      DBGBCRx_EL1_Value.E := 0;
      Set_DBGBCRx_EL1 (Hardware_Breakpoint_Id, DBGBCRx_EL1_Value);

      --  Set breakpoint target code address:
      DBGBVRx_EL1_Value := DBGBVRx_EL1_Type (To_Integer (Code_Address));
      Set_DBGBVRx_EL1 (Hardware_Breakpoint_Id, DBGBVRx_EL1_Value);

      --  Configure and enable breakpoint:
      DBGBCRx_EL1_Value.E := 1;
      DBGBCRx_EL1_Value.PMC := Breakpoint_Match_At_EL1_Or_EL0;
      DBGBCRx_EL1_Value.BT := Unlinked_Address_Match_Breakpoint;
      Set_DBGBCRx_EL1 (Hardware_Breakpoint_Id, DBGBCRx_EL1_Value);
   end Set_Hardware_Breakpoint;

   procedure Clear_Hardware_Breakpoint (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
   is
      DBGBCRx_EL1_Value : DBGBCRx_EL1_Type := Get_DBGBCRx_EL1 (Hardware_Breakpoint_Id);
   begin
      DBGBCRx_EL1_Value.E := 0;
      Set_DBGBCRx_EL1 (Hardware_Breakpoint_Id, DBGBCRx_EL1_Value);
   end Clear_Hardware_Breakpoint;

   function Query_Hardware_Breakpoint (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
      return Hardware_Breakpoint_Type
   is
      DBGBVRx_EL1_Value : constant DBGBVRx_EL1_Type := Get_DBGBVRx_EL1 (Hardware_Breakpoint_Id);
      DBGBCRx_EL1_Value : constant DBGBCRx_EL1_Type := Get_DBGBCRx_EL1 (Hardware_Breakpoint_Id);
   begin
      return (Target_Code_Addr => To_Address (Integer_Address (DBGBVRx_EL1_Value)),
              Enabled => DBGBCRx_EL1_Value.E = 1);
   end Query_Hardware_Breakpoint;

   procedure Set_Watchpoint (Watchpoint_Id : Watchpoint_Id_Type;
                             Data_Address : System.Address) is
      DBGWVRx_EL1_Value : DBGWVRx_EL1_Type;
      DBGWCRx_EL1_Value : DBGWCRx_EL1_Type;
   begin
      --  First disable watchpoint to be safe
      DBGWCRx_EL1_Value.E := 0;
      Set_DBGWCRx_EL1 (Watchpoint_Id, DBGWCRx_EL1_Value);

      --  Set watchpoint target data address:
      DBGWVRx_EL1_Value := DBGWVRx_EL1_Type (To_Integer (Data_Address));
      Set_DBGWVRx_EL1 (Watchpoint_Id, DBGWVRx_EL1_Value);

      --  Configure and enable watchpoint:
      DBGWCRx_EL1_Value.E := 1;
      DBGWCRx_EL1_Value.PAC := Watchpoint_Match_At_EL1_Or_EL0;
      DBGWCRx_EL1_Value.LSC := Watchpoint_Match_On_Load_Or_Store;
      DBGWCRx_EL1_Value.BAS := [others => 1];
      DBGWCRx_EL1_Value.WT := Unlinked_Address_Match_Watchpoint;
      Set_DBGWCRx_EL1 (Watchpoint_Id, DBGWCRx_EL1_Value);
   end Set_Watchpoint;

   procedure Clear_Watchpoint (Watchpoint_Id : Watchpoint_Id_Type) is
      DBGWCRx_EL1_Value : DBGWCRx_EL1_Type := Get_DBGWCRx_EL1 (Watchpoint_Id);
   begin
      DBGWCRx_EL1_Value.E := 0;
      Set_DBGWCRx_EL1 (Watchpoint_Id, DBGWCRx_EL1_Value);
   end Clear_Watchpoint;

   function Query_Watchpoint (Watchpoint_Id : Watchpoint_Id_Type)
      return Watchpoint_Type is
      DBGWVRx_EL1_Value : constant DBGWVRx_EL1_Type := Get_DBGWVRx_EL1 (Watchpoint_Id);
      DBGWCRx_EL1_Value : constant DBGWCRx_EL1_Type := Get_DBGWCRx_EL1 (Watchpoint_Id);
   begin
      return (Target_Data_Addr => To_Address (Integer_Address (DBGWVRx_EL1_Value)),
              Enabled => DBGWCRx_EL1_Value.E = 1);
   end Query_Watchpoint;

   procedure Enable_Single_Step_Exception is
      MDSCR_EL1_Value : MDSCR_EL1_Type := Get_MDSCR_EL1;
   begin
      MDSCR_EL1_Value.SS := 1;
      Set_MDSCR_EL1 (MDSCR_EL1_Value);
   end Enable_Single_Step_Exception;

   procedure Disable_Single_Step_Exception is
      MDSCR_EL1_Value : MDSCR_EL1_Type := Get_MDSCR_EL1;
   begin
      MDSCR_EL1_Value.SS := 0;
      Set_MDSCR_EL1 (MDSCR_EL1_Value);
   end Disable_Single_Step_Exception;

   procedure Set_OSLAR_EL1 (OSLAR_EL1_Value : OSLAR_EL1_Type) is
   begin
      System.Machine_Code.Asm (
         "msr oslar_el1, %0",
         Inputs => Interfaces.Unsigned_64'Asm_Input ("r", OSLAR_EL1_Value.Value), --  %0
         Volatile => True);
   end Set_OSLAR_EL1;

   function Get_OSLSR_EL1 return OSLSR_EL1_Type is
      OSLSR_EL1_Value : OSLSR_EL1_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, oslsr_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", OSLSR_EL1_Value.Value), --  %0
         Volatile => True);

      return OSLSR_EL1_Value;
   end Get_OSLSR_EL1;

   function Get_MDSCR_EL1 return MDSCR_EL1_Type is
      MDSCR_EL1_Value : MDSCR_EL1_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, mdscr_el1",
         Outputs => MDSCR_EL1_Type'Asm_Output ("=r", MDSCR_EL1_Value), --  %0
         Volatile => True);

      return MDSCR_EL1_Value;
   end Get_MDSCR_EL1;

   procedure Set_MDSCR_EL1 (MDSCR_EL1_Value : MDSCR_EL1_Type) is
   begin
      System.Machine_Code.Asm (
         "msr mdscr_el1, %0",
         Inputs => MDSCR_EL1_Type'Asm_Input ("r", MDSCR_EL1_Value), --  %0
         Volatile => True);
   end Set_MDSCR_EL1;

   function Get_ID_AA64DFR0_EL1 return ID_AA64DFR0_EL1_Type is
      ID_AA64DFR0_EL1_Value : ID_AA64DFR0_EL1_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, id_aa64dfr0_el1",
         Outputs => ID_AA64DFR0_EL1_Type'Asm_Output ("=r", ID_AA64DFR0_EL1_Value), --  %0
         Volatile => True);

      return ID_AA64DFR0_EL1_Value;
   end Get_ID_AA64DFR0_EL1;

   function Get_DBGBCRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
      return DBGBCRx_EL1_Type
   is
      DBGBCRx_EL1_Value : DBGBCRx_EL1_Type;
   begin
      case Hardware_Breakpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr0_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr1_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr2_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr3_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr4_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr5_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr6_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr7_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr8_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr9_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr10_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr11_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr12_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr13_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr14_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbcr15_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
      end case;

      return DBGBCRx_EL1_Value;
   end Get_DBGBCRx_EL1;

   procedure Set_DBGBCRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type;
                              DBGBCRx_EL1_Value : DBGBCRx_EL1_Type) is
   begin
      case Hardware_Breakpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "msr dbgbcr0_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "msr dbgbcr1_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "msr dbgbcr2_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "msr dbgbcr3_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "msr dbgbcr4_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "msr dbgbcr5_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "msr dbgbcr6_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "msr dbgbcr7_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "msr dbgbcr8_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "msr dbgbcr9_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "msr dbgbcr10_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "msr dbgbcr11_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "msr dbgbcr12_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "msr dbgbcr13_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "msr dbgbcr14_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "msr dbgbcr15_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGBCRx_EL1_Value.Value), --  %0
               Volatile => True);
      end case;
   end Set_DBGBCRx_EL1;

   function Get_DBGBVRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
      return DBGBVRx_EL1_Type is
      DBGBVRx_EL1_Value : DBGBVRx_EL1_Type;
   begin
      case Hardware_Breakpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr0_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr1_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr2_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr3_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr4_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr5_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr6_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr7_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr8_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr9_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr10_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr11_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr12_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr13_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr14_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgbvr15_el1",
               Outputs => DBGBVRx_EL1_Type'Asm_Output ("=r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
      end case;

      return DBGBVRx_EL1_Value;
   end Get_DBGBVRx_EL1;

   procedure Set_DBGBVRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type;
                              DBGBVRx_EL1_Value : DBGBVRx_EL1_Type) is
   begin
      case Hardware_Breakpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "msr dbgbvr0_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "msr dbgbvr1_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "msr dbgbvr2_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "msr dbgbvr3_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "msr dbgbvr4_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "msr dbgbvr5_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "msr dbgbvr6_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "msr dbgbvr7_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "msr dbgbvr8_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "msr dbgbvr9_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "msr dbgbvr10_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "msr dbgbvr11_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "msr dbgbvr12_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "msr dbgbvr13_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "msr dbgbvr14_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "msr dbgbvr15_el1, %0",
               Inputs => DBGBVRx_EL1_Type'Asm_Input ("r", DBGBVRx_EL1_Value), --  %0
               Volatile => True);
      end case;
   end Set_DBGBVRx_EL1;

   function Get_DBGWCRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type)
      return DBGWCRx_EL1_Type is
      DBGWCRx_EL1_Value : DBGWCRx_EL1_Type;
   begin
      case Watchpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr0_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr1_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr2_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr3_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr4_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr5_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr6_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr7_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr8_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr9_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr10_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr11_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr12_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr13_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr14_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwcr15_el1",
               Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
      end case;

      return DBGWCRx_EL1_Value;
   end Get_DBGWCRx_EL1;

   procedure Set_DBGWCRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type;
                              DBGWCRx_EL1_Value : DBGWCRx_EL1_Type) is
   begin
      case Watchpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "msr dbgwcr0_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "msr dbgwcr1_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "msr dbgwcr2_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "msr dbgwcr3_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "msr dbgwcr4_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "msr dbgwcr5_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "msr dbgwcr6_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "msr dbgwcr7_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "msr dbgwcr8_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "msr dbgwcr9_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "msr dbgwcr10_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "msr dbgwcr11_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "msr dbgwcr12_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "msr dbgwcr13_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "msr dbgwcr14_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "msr dbgwcr15_el1, %0",
               Inputs => Interfaces.Unsigned_64'Asm_Input ("r", DBGWCRx_EL1_Value.Value), --  %0
               Volatile => True);
      end case;
   end Set_DBGWCRx_EL1;

   function Get_DBGWVRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type)
      return DBGWVRx_EL1_Type is
      DBGWVRx_EL1_Value : DBGWVRx_EL1_Type;
   begin
      case Watchpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr0_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr1_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr2_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr3_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr4_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr5_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr6_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr7_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr8_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr9_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr10_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr11_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr12_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr13_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr14_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "mrs %0, dbgwvr15_el1",
               Outputs => DBGWVRx_EL1_Type'Asm_Output ("=r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
      end case;

      return DBGWVRx_EL1_Value;
   end Get_DBGWVRx_EL1;

   procedure Set_DBGWVRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type;
                              DBGWVRx_EL1_Value : DBGWVRx_EL1_Type) is
   begin
      case Watchpoint_Id is
         when 0 =>
            System.Machine_Code.Asm (
               "msr dbgwvr0_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 1 =>
            System.Machine_Code.Asm (
               "msr dbgwvr1_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 2 =>
            System.Machine_Code.Asm (
               "msr dbgwvr2_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 3 =>
            System.Machine_Code.Asm (
               "msr dbgwvr3_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 4 =>
            System.Machine_Code.Asm (
               "msr dbgwvr4_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 5 =>
            System.Machine_Code.Asm (
               "msr dbgwvr5_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 6 =>
            System.Machine_Code.Asm (
               "msr dbgwvr6_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 7 =>
            System.Machine_Code.Asm (
               "msr dbgwvr7_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 8 =>
            System.Machine_Code.Asm (
               "msr dbgwvr8_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 9 =>
            System.Machine_Code.Asm (
               "msr dbgwvr9_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 10 =>
            System.Machine_Code.Asm (
               "msr dbgwvr10_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 11 =>
            System.Machine_Code.Asm (
               "msr dbgwvr11_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 12 =>
            System.Machine_Code.Asm (
               "msr dbgwvr12_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 13 =>
            System.Machine_Code.Asm (
               "msr dbgwvr13_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 14 =>
            System.Machine_Code.Asm (
               "msr dbgwvr14_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
         when 15 =>
            System.Machine_Code.Asm (
               "msr dbgwvr15_el1, %0",
               Inputs => DBGWVRx_EL1_Type'Asm_Input ("r", DBGWVRx_EL1_Value), --  %0
               Volatile => True);
      end case;
   end Set_DBGWVRx_EL1;

end CPU.Self_Hosted_Debug;