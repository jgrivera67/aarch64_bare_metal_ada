--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary CPU Utilities
--

with System.Storage_Elements;
with Interfaces;

package CPU is
   use System.Storage_Elements;

   Instruction_Size_In_Bytes : constant := 4;

   Cache_Line_Size_In_Bytes : constant := 64;

   Page_Size_In_Bytes : constant := 4_096;

   function Get_Call_Address return System.Address with
      Inline_Always => False, Suppress => All_Checks;

   procedure Park_Cpu with
      Import,
      External_Name => "park_cpu", --  defined in utils_asm.S
      No_Return;

   function Cpu_In_Privileged_Mode return Boolean with
      Inline_Always, Suppress => All_Checks;

   procedure Break_Point with Inline_Always;

   procedure Memory_Barrier with
      Inline_Always;

   procedure Strong_Memory_Barrier  with
      Inline_Always;

   type Cpu_Register_Type is new Interfaces.Unsigned_64;

   function Count_Leading_Zeros (Value : Cpu_Register_Type) return Cpu_Register_Type with
      Post => Count_Leading_Zeros'Result <= Cpu_Register_Type'Size,
      Inline_Always, Suppress => All_Checks;

   -----------------------------------------------------------------------------
   --  Execution stack type declarations
   -----------------------------------------------------------------------------

   generic
      Stack_Size_In_Bytes : Integer_Address;
   package Generic_Execution_Stack is
      pragma Compile_Time_Error (
         Stack_Size_In_Bytes mod Page_Size_In_Bytes /= 0,
         "Stack size must be a multiple of the page size"
      );

      subtype Stack_Entry_Type is System.Storage_Elements.Integer_Address;

      Stack_Entries_Per_Page : constant :=
         Page_Size_In_Bytes / (Stack_Entry_Type'Size / System.Storage_Unit);

      type Stack_Overflow_Guard_Type is
         array (1 .. Stack_Entries_Per_Page) of Stack_Entry_Type with
            Size => Page_Size_In_Bytes * System.Storage_Unit,
            Alignment => Page_Size_In_Bytes;

      Stack_Size_In_Entries : constant Integer_Address :=
         Stack_Size_In_Bytes / (Stack_Entry_Type'Size / System.Storage_Unit);

      type Stack_Entries_Type is
         array (1 .. Stack_Size_In_Entries) of Stack_Entry_Type with
            Convention => C,
            Alignment  => Page_Size_In_Bytes;

      type Execution_Stack_Type is limited record
         Stack_Overflow_Guard : Stack_Overflow_Guard_Type := [others => 16#ada5_ada6_ada7_ada8#];
         Stack_Entries        : Stack_Entries_Type := [others => 16#ada1_ada2_ada3_ada4#];
      end record with
         Convention => C,
         Alignment  => Page_Size_In_Bytes;
   end Generic_Execution_Stack;

private

   -----------------------------------------------------------------------------
   --  CPU status register declarations
   -----------------------------------------------------------------------------

   type SP_Selector_Type is (
      SP_EL0, --  Thread context stack pointer
      SP_ELx  --  Interrupt context stack pointer
   ) with Size => 1;

   for SP_Selector_Type use (
      SP_EL0 => 2#0#,
      SP_ELx => 2#1#
   );

   type Exception_Level_Type is (
      EL0,
      EL1,
      EL2,
      EL3
   ) with Size => 2;

   for Exception_Level_Type use (
      EL0 => 2#0#,
      EL1 => 2#1#,
      EL2 => 2#10#,
      EL3 => 2#11#
   );

   type Execution_State_Type is (
      Execution_State_AArch64,
      Execution_State_AArch32
   ) with Size => 1;

   for Execution_State_Type use (
      Execution_State_AArch64 => 2#0#,
      Execution_State_AArch32 => 2#1#
   );

   type Interrupt_Flag_Type is
      (Interrupt_Enabled,
       Interrupt_Disabled)
   with Size => 1;

   for Interrupt_Flag_Type use (
      Interrupt_Enabled => 2#0#,
      Interrupt_Disabled => 2#1#
   );

   type DAIF_Type is record
      F : Interrupt_Flag_Type := Interrupt_Disabled;
      I : Interrupt_Flag_Type := Interrupt_Disabled;
      A : Interrupt_Flag_Type := Interrupt_Disabled;
      D : Interrupt_Flag_Type := Interrupt_Disabled;
   end record with
     Size => 4,
     Bit_Order => System.Low_Order_First;

   for DAIF_Type use record
      F at 0 range 0 .. 0;
      I at 0 range 1 .. 1;
      A at 0 range 2 .. 2;
      D at 0 range 3 .. 3;
   end record;

   type Arithmetic_Flag_Type is new Boolean
       with Size => 1;

   type NZCV_Type is record
      V : Arithmetic_Flag_Type := False;
      C : Arithmetic_Flag_Type := False;
      Z : Arithmetic_Flag_Type := False;
      N : Arithmetic_Flag_Type := False;
   end record with Size => 4, Bit_Order => System.Low_Order_First;

   for NZCV_Type use record
      V at 0 range 0 .. 0;
      C at 0 range 1 .. 1;
      Z at 0 range 2 .. 2;
      N at 0 range 3 .. 3;
   end record;

   --
   --  PSTATE register
   --
   --  NOTE: We don't need to declare this register with Volatile_Full_Access,
   --  as it is not memory-mapped. It is accessed via MRS/MSR instructions.
   --
   type PSTATE_Type (As_Value : Boolean := True) is record
      case As_Value is
         when True =>
            Value : Cpu_Register_Type := 0;
         when False =>
            SPSel : SP_Selector_Type := SP_EL0;
            CurrentEL : Exception_Level_Type := EL0;
            M : Execution_State_Type := Execution_State_AArch64;
            DAIF : DAIF_Type;
            NZCV : NZCV_Type;
      end case;
   end record with
     Size => 64, Bit_Order => System.Low_Order_First, Unchecked_Union;

   for PSTATE_Type use record
      SPSel      at 0 range  0 .. 0;
      CurrentEL  at 0 range  2 .. 3;
      M          at 0 range  4 .. 4;
      DAIF       at 0 range  6 .. 9;
      NZCV       at 0 range 28 .. 31;
      Value      at 0 range  0 .. 63;
   end record;

   function Get_CurrentEL return Exception_Level_Type;

   function Get_DAIF return DAIF_Type;

   use type Interfaces.Unsigned_8;

   --
   --  Bit masks to use with msr DAIFset/DAIFclr:
   --
   DAIF_SetClr_F_Bit_Mask : constant Interfaces.Unsigned_8 := 2#1#; --  bit 0
   DAIF_SetClr_I_Bit_Mask : constant Interfaces.Unsigned_8 := 2#10#; --  bit 1
   DAIF_SetClr_A_Bit_Mask : constant Interfaces.Unsigned_8 := 2#100#; --  bit 2
   DAIF_SetClr_D_Bit_Mask : constant Interfaces.Unsigned_8 := 2#1000#; --  bit 3
   DAIF_SetClr_IF_Mask : constant Interfaces.Unsigned_8 := (DAIF_SetClr_I_Bit_Mask or DAIF_SetClr_F_Bit_Mask);

   function Cpu_In_Privileged_Mode return Boolean is
      (Get_CurrentEL /= EL0);

-----------------------------------------------------------------------------
   --  SCTLR_EL1 register declarations
   -----------------------------------------------------------------------------

   type MMU_Enable_Type is
      (MMU_Disabled,
       MMU_Enabled)
   with Size => 1;

   for MMU_Enable_Type use
     (MMU_Disabled => 2#0#,
      MMU_Enabled => 2#1#);

   type Alignment_Check_Enable_Type is (Alignment_Check_Disabled,
                                        Alignment_Check_Enabled)
   with Size => 1;

   for Alignment_Check_Enable_Type use
     (Alignment_Check_Disabled => 2#0#,
      Alignment_Check_Enabled => 2#1#);

   type Cacheability_Control_Type is (Non_Cacheable,
                                      Cacheable)
   with Size => 1;

   for Cacheability_Control_Type use
     (Non_Cacheable => 2#0#,
      Cacheable => 2#1#);

   type SP_EL1_Alignment_Check_Enable_Type is (SP_EL1_Alignment_Check_Disabled,
                                               SP_EL1_Alignment_Check_Enabled)
   with Size => 1;

   for SP_EL1_Alignment_Check_Enable_Type use
     (SP_EL1_Alignment_Check_Disabled => 2#0#,
      SP_EL1_Alignment_Check_Enabled => 2#1#);

   type SP_EL0_Alignment_Check_Enable_Type is (SP_EL0_Alignment_Check_Disabled,
                                               SP_EL0_Alignment_Check_Enabled)
   with Size => 1;

   for SP_EL0_Alignment_Check_Enable_Type use
     (SP_EL0_Alignment_Check_Disabled => 2#0#,
      SP_EL0_Alignment_Check_Enabled => 2#1#);

   type User_Mask_Access_Enable_Type is (User_Mask_Access_Disabled,
                                         User_Mask_Access_Enabled)
   with Size => 1;

   for User_Mask_Access_Enable_Type use
     (User_Mask_Access_Disabled => 2#0#,
      User_Mask_Access_Enabled => 2#1#);

   type Instruction_Access_Cacheability_Control_Type is (
      Instruction_Access_Non_Cacheable,
      Instruction_Access_Cacheable)
   with Size => 1;

   for Instruction_Access_Cacheability_Control_Type use
     (Instruction_Access_Non_Cacheable => 2#0#,
      Instruction_Access_Cacheable => 2#1#);

   type EL0_WFI_Trap_Disable_Type is (EL0_WFI_Trap_Enabled,
                                      EL0_WFI_Trap_Disabled)
   with Size => 1;

   for EL0_WFI_Trap_Disable_Type use
     (EL0_WFI_Trap_Enabled => 2#0#,
      EL0_WFI_Trap_Disabled => 2#1#);

   type EL0_WFE_Trap_Disable_Type is (EL0_WFE_Trap_Enabled,
                                      EL0_WFE_Trap_Disabled)
   with Size => 1;

   for EL0_WFE_Trap_Disable_Type use
     (EL0_WFE_Trap_Enabled => 2#0#,
      EL0_WFE_Trap_Disabled => 2#1#);

   type Write_Permission_Implies_XN_Enable_Type is
      (Write_Permission_Implies_XN_Disabled,
       Write_Permission_Implies_XN_Enabled)
   with Size => 1;

   for Write_Permission_Implies_XN_Enable_Type use
     (Write_Permission_Implies_XN_Disabled => 2#0#,
      Write_Permission_Implies_XN_Enabled => 2#1#);

   type EL1_Endianness_Type is
      (EL1_Is_Little_Endian,
       EL1_Is_Big_Endian)
   with Size => 1;

   for EL1_Endianness_Type use
     (EL1_Is_Little_Endian => 2#0#,
      EL1_Is_Big_Endian => 2#1#);

   type EL0_Endianness_Type is
      (EL0_Is_Little_Endian,
       EL0_Is_Big_Endian)
   with Size => 1;

   for EL0_Endianness_Type use
     (EL0_Is_Little_Endian => 2#0#,
      EL0_Is_Big_Endian => 2#1#);

   type APDBKey_EL1_Pointer_Authentication_Enable_Type is
      (APDBKey_EL1_Pointer_Authentication_Disabled,
       APDBKey_EL1_Pointer_Authentication_Enabled)
   with Size => 1;

   for APDBKey_EL1_Pointer_Authentication_Enable_Type use
     (APDBKey_EL1_Pointer_Authentication_Disabled => 2#0#,
      APDBKey_EL1_Pointer_Authentication_Enabled => 2#1#);

   type APDAKey_EL1_Pointer_Authentication_Enable_Type is
      (APDAKey_EL1_Pointer_Authentication_Disabled,
       APDAKey_EL1_Pointer_Authentication_Enabled)
   with Size => 1;

   for APDAKey_EL1_Pointer_Authentication_Enable_Type use
     (APDAKey_EL1_Pointer_Authentication_Disabled => 2#0#,
      APDAKey_EL1_Pointer_Authentication_Enabled => 2#1#);

   type APIBKey_EL1_Pointer_Authentication_Enable_Type is
      (APIBKey_EL1_Pointer_Authentication_Disabled,
       APIBKey_EL1_Pointer_Authentication_Enabled)
   with Size => 1;

   for APIBKey_EL1_Pointer_Authentication_Enable_Type use
     (APIBKey_EL1_Pointer_Authentication_Disabled => 2#0#,
      APIBKey_EL1_Pointer_Authentication_Enabled => 2#1#);

   type APIAKey_EL1_Pointer_Authentication_Enable_Type is
      (APIAKey_EL1_Pointer_Authentication_Disabled,
       APIAKey_EL1_Pointer_Authentication_Enabled)
   with Size => 1;

   for APIAKey_EL1_Pointer_Authentication_Enable_Type use
     (APIAKey_EL1_Pointer_Authentication_Disabled => 2#0#,
      APIAKey_EL1_Pointer_Authentication_Enabled => 2#1#);

   --
   --  System control register for EL1
   --
   --  NOTE: We don't need to declare this register with Volatile_Full_Access,
   --  as it is not memory-mapped. It is accessed via MRS/MSR instructions.
   --
   type SCTLR_EL1_Type is record
      M : MMU_Enable_Type := MMU_Disabled;
      A : Alignment_Check_Enable_Type := Alignment_Check_Disabled;
      C : Cacheability_Control_Type := Non_Cacheable;
      SA : SP_EL1_Alignment_Check_Enable_Type := SP_EL1_Alignment_Check_Disabled;
      SA0 : SP_EL0_Alignment_Check_Enable_Type := SP_EL0_Alignment_Check_Disabled;
      UMA : User_Mask_Access_Enable_Type := User_Mask_Access_Disabled;
      I : Instruction_Access_Cacheability_Control_Type := Instruction_Access_Non_Cacheable;
      EnDB : APDBKey_EL1_Pointer_Authentication_Enable_Type := APDBKey_EL1_Pointer_Authentication_Disabled;
      nTWI : EL0_WFI_Trap_Disable_Type := EL0_WFI_Trap_Enabled;
      nTWE : EL0_WFE_Trap_Disable_Type := EL0_WFE_Trap_Enabled;
      WXN : Write_Permission_Implies_XN_Enable_Type := Write_Permission_Implies_XN_Disabled;
      E0E : EL0_Endianness_Type := EL0_Is_Little_Endian;
      EE : EL1_Endianness_Type := EL1_Is_Little_Endian;
      EnDA : APDAKey_EL1_Pointer_Authentication_Enable_Type := APDAKey_EL1_Pointer_Authentication_Disabled;
      EnIB : APIBKey_EL1_Pointer_Authentication_Enable_Type := APIBKey_EL1_Pointer_Authentication_Disabled;
      EnIA : APIAKey_EL1_Pointer_Authentication_Enable_Type := APIAKey_EL1_Pointer_Authentication_Disabled;
   end record
   with Size => 64,
        Bit_Order => System.Low_Order_First;

   for SCTLR_EL1_Type use record
      M at 0 range 0 .. 0;
      A at 0 range 1 .. 1;
      C at 0 range 2 .. 2;
      SA at 0 range 3 .. 3;
      SA0 at 0 range 4 .. 4;
      UMA at 0 range 9 .. 9;
      I at 0 range 12 .. 12;
      EnDB at 0 range 13 .. 13;
      nTWI at 0 range 16 .. 16;
      nTWE at 0 range 18 .. 18;
      WXN at 0 range 19 .. 19;
      E0E at 0 range 24 .. 24;
      EE at 0 range 25 .. 25;
      EnDA at 0 range 27 .. 27;
      EnIB at 0 range 30 .. 30;
      EnIA at 0 range 31 .. 31;
   end record;

   function Get_SCTLR_EL1 return SCTLR_EL1_Type;

   procedure Set_SCTLR_EL1 (SCTLR_EL1_Value : SCTLR_EL1_Type);

end CPU;
