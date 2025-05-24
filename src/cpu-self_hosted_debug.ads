--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

package CPU.Self_Hosted_Debug with SPARK_Mode => On is
   Max_Num_Hardware_Breakpoints : constant := 16;

   type Num_Hardware_Breakpoints_Type is range 0 .. Max_Num_Hardware_Breakpoints;

   Max_Num_Watchpoints : constant := 16;

   type Num_Watchpoints_Type is range 0 .. Max_Num_Watchpoints;

   type Self_Hosted_Debug_Capabilities_Type is record
      Num_Hardware_Breakpoints : Num_Hardware_Breakpoints_Type;
      Num_Watchpoints : Num_Watchpoints_Type;
   end record;

   type Hardware_Breakpoint_Id_Type is range 0 .. Num_Hardware_Breakpoints_Type'Last - 1;

   type Watchpoint_Id_Type is range 0 .. Num_Watchpoints_Type'Last - 1;

   type Debug_Event_Type is (Hardware_Breakpoint_Event,
                             Software_Breakpoint_Event,
                             Watchpoint_Event,
                             Single_Step_Event);

   procedure Run_Debugger (Debug_Event : Debug_Event_Type)
      with Pre => Cpu_In_Privileged_Mode and then
                  Cpu_Interrupting_Disabled;

   type Hardware_Breakpoint_Type is record
      Target_Code_Addr : System.Address := System.Null_Address;
      Enabled : Boolean := False;
   end record;

   type Watchpoint_Type is record
      Target_Data_Addr : System.Address := System.Null_Address;
      Enabled : Boolean := False;
   end record;

   function Get_Self_Hosted_Debug_Capabilities return Self_Hosted_Debug_Capabilities_Type;

   procedure Enable_Self_Hosted_Debugging with
      Pre => Cpu_In_Privileged_Mode and then
             Cpu_Interrupting_Disabled;

   procedure Disable_Self_Hosted_Debugging with
      Pre => Cpu_In_Privileged_Mode and then
             Cpu_Interrupting_Disabled;

   procedure Set_Hardware_Breakpoint (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type;
                                      Code_Address : System.Address) with
      Pre => Cpu_In_Privileged_Mode and then
             Cpu_Interrupting_Disabled and then
             To_Integer (Code_Address) mod CPU.Instruction_Size_In_Bytes = 0;

   procedure Clear_Hardware_Breakpoint (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
      with Pre => Cpu_In_Privileged_Mode and then
                  Cpu_Interrupting_Disabled;

   function Query_Hardware_Breakpoint (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
      return Hardware_Breakpoint_Type;

   procedure Set_Watchpoint (Watchpoint_Id : Watchpoint_Id_Type;
                             Data_Address : System.Address) with
      Pre => Cpu_In_Privileged_Mode and then
             Cpu_Interrupting_Disabled and then
             To_Integer (Data_Address) mod (Interfaces.Unsigned_64'Size / System.Storage_Unit) = 0;

   procedure Clear_Watchpoint (Watchpoint_Id : Watchpoint_Id_Type)
      with Pre => Cpu_In_Privileged_Mode and then
                  Cpu_Interrupting_Disabled;

   function Query_Watchpoint (Watchpoint_Id : Watchpoint_Id_Type)
      return Watchpoint_Type;

   procedure Enable_Single_Step_Exception
      with Pre => Cpu_In_Privileged_Mode and then
                  Cpu_Interrupting_Disabled;
   procedure Disable_Single_Step_Exception
      with Pre => Cpu_In_Privileged_Mode and then
                  Cpu_Interrupting_Disabled;

private

   --
   --  OS Lock Access Register for EL1
   --
   type OSLAR_EL1_Type (As_Value : Boolean := True)  is record
      case As_Value is
         when True =>
            Value : Interfaces.Unsigned_64 := 0;
         when False =>
            OSLK : Bit_Type := 2#0#;
      end case;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First,
        Unchecked_Union;

   for OSLAR_EL1_Type use record
      Value at 0 range 0 .. 63;
      OSLK at 0 range 0 .. 0;
   end record;

   procedure Set_OSLAR_EL1 (OSLAR_EL1_Value : OSLAR_EL1_Type);

   --
   --  OS Lock Status Register for EL1
   --
   type OSLSR_EL1_Type (As_Value : Boolean := True)  is record
      case As_Value is
         when True =>
            Value : Interfaces.Unsigned_64 := 0;
         when False =>
            OSLK : Bit_Type := 2#0#;
      end case;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First,
        Unchecked_Union;

   for OSLSR_EL1_Type use record
      Value at 0 range 0 .. 63;
      OSLK at 0 range 1 .. 1;
   end record;

   function Get_OSLSR_EL1 return OSLSR_EL1_Type;

   --
   --  Monitor Debug System Control register for EL1
   --
   --  NOTE: We don't need to declare this register with Volatile_Full_Access,
   --  as it is not memory-mapped. It is accessed via MRS/MSR instructions.
   --
   type MDSCR_EL1_Type is record
      SS  : Bit_Type := 2#0#;
      KDE : Bit_Type := 2#0#;
      MDE : Bit_Type := 2#0#;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First;

   for MDSCR_EL1_Type use record
      SS  at 0 range 0 .. 0;
      KDE at 0 range 13 .. 13;
      MDE at 0 range 15 .. 15;
   end record;

   function Get_MDSCR_EL1 return MDSCR_EL1_Type;

   procedure Set_MDSCR_EL1 (MDSCR_EL1_Value : MDSCR_EL1_Type);

   --
   --  AArch64 Debug Feature Register 0
   --
   type ID_AA64DFR0_EL1_Type is record
      BRPs : Four_Bits_Type := 2#0#;
      WRPs : Four_Bits_Type := 2#0#;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First;

   for ID_AA64DFR0_EL1_Type use record
      BRPs at 0 range 12 .. 15;
      WRPs at 0 range 20 .. 23;
   end record;

   function Get_ID_AA64DFR0_EL1 return ID_AA64DFR0_EL1_Type;

   -----------------------------------------------------------------------------
   --  Hardware Breakpoint Control Register Declarations
   -----------------------------------------------------------------------------

   type DBGBCRx_EL1_PMC_Type is (
      Breakpoint_Match_At_No_Level,
      Breakpoint_Match_At_EL1,
      Breakpoint_Match_At_EL0,
      Breakpoint_Match_At_EL1_Or_EL0
   ) with Size => 2;

   for DBGBCRx_EL1_PMC_Type use (
      Breakpoint_Match_At_No_Level => 2#00#,
      Breakpoint_Match_At_EL1 => 2#01#,
      Breakpoint_Match_At_EL0 => 2#10#,
      Breakpoint_Match_At_EL1_Or_EL0 => 2#11#
   );

   type DBGBCRx_EL1_BT_Type is (
      Unlinked_Address_Match_Breakpoint,
      Linked_Address_Match_Breakpoint,
      Unlinked_Context_Id_Match_Breakpoint,
      Linked_Context_Id_Match_Breakpoint,
      Unlinked_Address_Mismatch_Breakpoint,
      Linked_Address_Mismatch_Breakpoint
   ) with Size => 4;

   for DBGBCRx_EL1_BT_Type use (
      Unlinked_Address_Match_Breakpoint => 16#0#,
      Linked_Address_Match_Breakpoint => 16#1#,
      Unlinked_Context_Id_Match_Breakpoint => 16#2#,
      Linked_Context_Id_Match_Breakpoint => 16#3#,
      Unlinked_Address_Mismatch_Breakpoint => 16#4#,
      Linked_Address_Mismatch_Breakpoint => 16#5#
   );

   --
   --  Hardware Breakpoint Control Register
   --
   type DBGBCRx_EL1_Type (As_Value : Boolean := True)  is record
      case As_Value is
         when True =>
            Value : Interfaces.Unsigned_64 := 0;
         when False =>
            E : Bit_Type := 0;
            PMC : DBGBCRx_EL1_PMC_Type := Breakpoint_Match_At_No_Level;
            BT : DBGBCRx_EL1_BT_Type := Unlinked_Address_Match_Breakpoint;
      end case;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First,
        Unchecked_Union;

   for DBGBCRx_EL1_Type use record
      Value at 0 range 0 .. 63;
      E  at 0 range 0 .. 0;
      PMC at 0 range 1 .. 2;
      BT at 0 range 20 .. 23;
   end record;

   function Get_DBGBCRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
      return DBGBCRx_EL1_Type with
      Pre => Cpu_In_Privileged_Mode;

   procedure Set_DBGBCRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type;
                              DBGBCRx_EL1_Value : DBGBCRx_EL1_Type) with
      Pre => Cpu_In_Privileged_Mode;

   -----------------------------------------------------------------------------
   --  Hardware Breakpoint Value Register Declarations
   -----------------------------------------------------------------------------

   type DBGBVRx_EL1_Type is new Interfaces.Unsigned_64;

   function Get_DBGBVRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type)
      return DBGBVRx_EL1_Type with
      Pre => Cpu_In_Privileged_Mode;

   procedure Set_DBGBVRx_EL1 (Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type;
                              DBGBVRx_EL1_Value : DBGBVRx_EL1_Type) with
      Pre => Cpu_In_Privileged_Mode;

   -----------------------------------------------------------------------------
   --  Watchpoint Control Register Declarations
   -----------------------------------------------------------------------------

   type DBGWCRx_EL1_PAC_Type is (
      Watchpoint_Match_At_No_Level,
      Watchpoint_Match_At_EL1,
      Watchpoint_Match_At_EL0,
      Watchpoint_Match_At_EL1_Or_EL0
   ) with Size => 2;

   for DBGWCRx_EL1_PAC_Type use (
      Watchpoint_Match_At_No_Level => 2#00#,
      Watchpoint_Match_At_EL1 => 2#01#,
      Watchpoint_Match_At_EL0 => 2#10#,
      Watchpoint_Match_At_EL1_Or_EL0 => 2#11#
   );

   type DBGWCRx_EL1_LSC_Type is (
      Watchpoint_Match_On_No_Instruction,
      Watchpoint_Match_On_Load,
      Watchpoint_Match_On_Store,
      Watchpoint_Match_On_Load_Or_Store
   ) with Size => 2;

   for DBGWCRx_EL1_LSC_Type use (
      Watchpoint_Match_On_No_Instruction => 2#00#,
      Watchpoint_Match_On_Load => 2#01#,
      Watchpoint_Match_On_Store => 2#10#,
      Watchpoint_Match_On_Load_Or_Store => 2#11#
   );

   --  Byte Address Select (1 bit for each byte in a double word)
   type DBGWCRx_EL1_BAS_Type is array (0 .. 7) of Bit_Type
      with Component_Size => 1, Size => 8;

   type DBGWCRx_EL1_WT_Type is (
      Unlinked_Address_Match_Watchpoint,
      Linked_Address_Match_Watchpoint
   ) with Size => 1;

   for DBGWCRx_EL1_WT_Type use (
      Unlinked_Address_Match_Watchpoint => 2#0#,
      Linked_Address_Match_Watchpoint => 2#1#
   );

   --
   --  Watchpoint Control Register
   --
   type DBGWCRx_EL1_Type (As_Value : Boolean := True)  is record
      case As_Value is
         when True =>
            Value : Interfaces.Unsigned_64 := 0;
         when False =>
            E : Bit_Type := 0;
            PAC : DBGWCRx_EL1_PAC_Type := Watchpoint_Match_At_No_Level;
            LSC : DBGWCRx_EL1_LSC_Type := Watchpoint_Match_On_No_Instruction;
            BAS : DBGWCRx_EL1_BAS_Type := [others => 0];
            WT : DBGWCRx_EL1_WT_Type := Unlinked_Address_Match_Watchpoint;
      end case;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First,
        Unchecked_Union;

   for DBGWCRx_EL1_Type use record
      Value at 0 range 0 .. 63;
      E  at 0 range 0 .. 0;
      PAC at 0 range 1 .. 2;
      LSC at 0 range 3 .. 4;
      BAS at 0 range 5 .. 12;
      WT at 0 range 20 .. 20;
   end record;

   function Get_DBGWCRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type)
      return DBGWCRx_EL1_Type with
      Pre => Cpu_In_Privileged_Mode;

   procedure Set_DBGWCRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type;
                              DBGWCRx_EL1_Value : DBGWCRx_EL1_Type) with
      Pre => Cpu_In_Privileged_Mode;

   -----------------------------------------------------------------------------
   --  Watchpoint Value Register Declarations
   -----------------------------------------------------------------------------

   type DBGWVRx_EL1_Type is new Interfaces.Unsigned_64;

   function Get_DBGWVRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type)
      return DBGWVRx_EL1_Type with
      Pre => Cpu_In_Privileged_Mode;

   procedure Set_DBGWVRx_EL1 (Watchpoint_Id : Watchpoint_Id_Type;
                              DBGWVRx_EL1_Value : DBGWVRx_EL1_Type) with
      Pre => Cpu_In_Privileged_Mode;

end CPU.Self_Hosted_Debug;