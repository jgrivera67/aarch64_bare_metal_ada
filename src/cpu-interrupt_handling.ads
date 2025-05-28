--
--  Copyright (c) 2022-2023, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Bit_Sized_Integer_Types;
with Interrupt_Controller_Driver;
with System;
with Interfaces;

package CPU.Interrupt_Handling with SPARK_Mode => On is

   procedure Initialize with
     Pre  => Cpu_In_Privileged_Mode and then
             Cpu_Interrupting_Disabled,
     Post => not Cpu_Interrupting_Disabled;

   function Get_Saved_PC return System.Address;

   procedure Set_Saved_PC (PC_Value : System.Address);

   procedure Wait_For_Interrupt with
      Pre => Cpu_In_Privileged_Mode,
      Inline_Always;

   --
   --  Disable interrupts at the CPU
   --
   --  NOTE: Only the IRQ interrupt is disabled, not the FIQ interrupt.
   --
   pragma Warnings (Off, "postcondition does not mention function result");
   function Disable_Cpu_Interrupting return Cpu_Register_Type with
      Pre => Cpu_In_Privileged_Mode,
      Post => Cpu_Interrupting_Disabled;
   pragma Warnings (On, "postcondition does not mention function result");

   --
   --  Restore interrupt enablement at the CPU
   --
   procedure Restore_Cpu_Interrupting (Old_Cpu_Interrupting : Cpu_Register_Type) with
      Pre => Cpu_In_Privileged_Mode and then
             Cpu_Interrupting_Disabled;

   procedure Enable_Cpu_Interrupting with
      Pre => Cpu_In_Privileged_Mode,
      Post => not Cpu_Interrupting_Disabled;

   function Cpu_In_Interrupt_Context return Boolean;

   -----------------------------------------------------------------------------
   --  Cpu Context saved/restored on interrupt entry/exit
   -----------------------------------------------------------------------------

   --
   --  CPU integer registers
   --
   --  NOTE: The order of registers in this enumerated type must match the order
   --  in which CPU registers are stored in the stack when entering an ISR
   --  (pushed in reverse order and popped in this order).
   --
   type Cpu_Register_Id_Type is (ELR_ELx, SPSR_ELx,
                                 X30_Or_LR, SP,
                                 X28, X29_Or_FP,
                                 X26, X27,
                                 X24, X25,
                                 X22, X23,
                                 X20, X21,
                                 X18, X19,
                                 X16, X17,
                                 X14, X15,
                                 X12, X13,
                                 X10, X11,
                                 X8, X9,
                                 X6, X7,
                                 X4, X5,
                                 X2, X3,
                                 X0, X1);

   type Cpu_Register_Array_Type is array (Cpu_Register_Id_Type) of Cpu_Register_Type;

   type Cpu_Context_Type is record
      Registers : Cpu_Register_Array_Type;
   end record
      with Convention => C;

   for Cpu_Context_Type use record
      Registers at 16#00# range 0 .. (64 * Cpu_Register_Array_Type'Length) - 1;
   end record;

   pragma Compile_Time_Error (
      (Cpu_Context_Type'Size / System.Storage_Unit) mod CPU.Stack_Alignment_In_Bytes /= 0,
      "Cpu_Context_Type has the wrong size");

   -----------------------------------------------------------------------------
   --  ISR stacks
   -----------------------------------------------------------------------------

   --
   --  NOTE: The Value of this constant must match the value of
   --  ISR_STACK_SIZE_IN_BYTES in cpu_asm.h
   --
   ISR_Stack_Size_In_Bytes : constant := 4 * Page_Size_In_Bytes;
   package ISR_Stacks_Package is new
      Generic_Execution_Stack (Stack_Size_In_Bytes => ISR_Stack_Size_In_Bytes);

   ISR_Stack_Size_In_Entries : constant Integer_Address :=
      ISR_Stack_Size_In_Bytes /
      (ISR_Stacks_Package.Stack_Entry_Type'Size / System.Storage_Unit);

   ISR_Stacks : array (Valid_Cpu_Core_Id_Type) of
      ISR_Stacks_Package.Execution_Stack_Type with
         Linker_Section => ".isr_stacks",
         Convention => C,
         Export,
         External_Name => "isr_stacks";

   -----------------------------------------------------------------------------
   --  Interrupts
   -----------------------------------------------------------------------------

   Generic_Virtual_Timer_Interrupt_Id : constant
      Interrupt_Controller_Driver.Internal_Interrupt_Id_Type := 27;

   use type Interrupt_Controller_Driver.External_Interrupt_Id_Type;

   UART0_Interrupt_Id : constant
      Interrupt_Controller_Driver.External_Interrupt_Id_Type :=
         --  Value taken from device tree (third_party/bcm2712_rpi5.dts and
         --  third_party/bcm2711_rpi4_b.dts)
         Interrupt_Controller_Driver.External_Interrupt_Id_Type'First + 16#79#;

   use type Interrupt_Controller_Driver.Valid_Interrupt_Priority_Type;

   Interrupt_Priorities : constant
      array (Interrupt_Controller_Driver.Valid_Interrupt_Id_Type) of
         Interrupt_Controller_Driver.Interrupt_Priority_Type :=
      [Generic_Virtual_Timer_Interrupt_Id =>
         Interrupt_Controller_Driver.Highest_Interrupt_Priority + 1,
       UART0_Interrupt_Id =>
         Interrupt_Controller_Driver.Lowest_Interrupt_Priority - 1,
       others =>
         Interrupt_Controller_Driver.Lowest_Interrupt_Priority];

      type Interrupt_Nesting_Type is private;

      function Get_Cpu_Interrupt_Nesting (Cpu_Id : Valid_Cpu_Core_Id_Type)
         return Interrupt_Nesting_Type;

      function Get_Interrupt_Nesting_Stack_Pointer (
         Interrupt_Nesting : Interrupt_Nesting_Type)
         return System.Address;

private

   -----------------------------------------------------------------------------
   --  Subprograms invoked from Assembly code
   -----------------------------------------------------------------------------

   procedure Ada_Handle_EL1_Synchronous_Exception
      with Export,
           Convention => C,
           External_Name => "ada_handle_el1_synchronous_exception";

   procedure Ada_Handle_EL1_Irq_Interrupt
      with Export,
           Convention => C,
           External_Name => "ada_handle_el1_irq_interrupt";

   procedure Ada_Handle_EL1_Fiq_Interrupt
      with Export,
           Convention => C,
           External_Name => "ada_handle_el1_fiq_interrupt";

   procedure Ada_Handle_EL1_SError_Exception
      with Export,
           Convention => C,
           External_Name => "ada_handle_el1_serror_exception",
           No_Return;

   procedure Ada_Handle_EL1_Unexpected_Exception
      with Export,
           Convention => C,
           External_Name => "ada_handle_el1_unexpected_exception",
           No_Return;

   function Ada_Enter_Interrupt_Context (Stack_Pointer : System.Address) return System.Address
      with Export,
           Convention => C,
           External_Name => "ada_enter_interrupt_context";

   function Ada_Exit_Interrupt_Context (Stack_Pointer : System.Address) return System.Address
      with Export,
           Convention => C,
           External_Name => "ada_exit_interrupt_context";

   -----------------------------------------------------------------------------
   --  Exception Registers Declarations
   -----------------------------------------------------------------------------

   type ESR_EL1_IFSC_Type is (ESR_EL1_IFSC_Address_Size_Fault_TTBRx,
                              ESR_EL1_IFSC_Translation_Fault_1st_Level,
                              ESR_EL1_IFSC_Translation_Fault_2nd_Level)
      with Size => 6;

   for ESR_EL1_IFSC_Type use (ESR_EL1_IFSC_Address_Size_Fault_TTBRx => 2#000000#,
                              ESR_EL1_IFSC_Translation_Fault_1st_Level => 2#000101#,
                              ESR_EL1_IFSC_Translation_Fault_2nd_Level => 2#000110#);

   type ESR_EL1_EA_Type is (ESR_EL1_EA_External_Abort_Marked_DECERR,
                            ESR_EL1_EA_External_Abort_Marked_SLVERR)
      with Size => 1;

   for ESR_EL1_EA_Type use (ESR_EL1_EA_External_Abort_Marked_DECERR => 2#0#,
                            ESR_EL1_EA_External_Abort_Marked_SLVERR => 2#1#);

   --
   --  NOTE: The actual size of this type should be 24 bits, but Ada does not allow to define
   --  a variant record of 24 bits
   --
   type ESR_EL1_ISS_Type (As_Value : Boolean := True) is record
      case As_Value is
         when True =>
            Value : Bit_Sized_Integer_Types.Twenty_Four_Bits_Type := 0;
         when False =>
            INST_ABORT_IFSC : ESR_EL1_IFSC_Type;
            INST_ABORT_EA : ESR_EL1_EA_Type;
      end case;
   end record with
      Size => 32, Bit_Order => System.Low_Order_First, Unchecked_Union;

   for ESR_EL1_ISS_Type use record
      Value at 0 range 0 .. 24;
      INST_ABORT_IFSC at 0 range 0 .. 5;
      INST_ABORT_EA at 0 range 9 .. 9;
   end record;

   type ESR_EL1_EC_Type is (ESR_EL1_EC_Unknown,
                            ESR_EL1_EC_Trapped_WFI_WFE,
                            ESR_EL1_EC_Trapped_Access_SME_SVE_Advanced_SIMD_FP,
                            ESR_EL1_EC_Illegal_State,
                            ESR_EL1_EC_Aarch64_SVC,
                            ESR_EL1_EC_Trapped_MSR_MRS_System_Inst_In_AArch64,
                            ESR_EL1_EC_Instruction_Abort_Lower_EL,
                            ESR_EL1_EC_Instruction_Abort_Current_EL,
                            ESR_EL1_EC_PC_ALignment_Fault,
                            ESR_EL1_EC_Data_Abort_Lower_EL,
                            ESR_EL1_EC_Data_Abort_Current_EL,
                            ESR_EL1_EC_SP_Alignment_Fault,
                            ESR_EL1_EC_Trapped_Floating_Point_Exception,
                            ESR_EL1_EC_SError_Exception,
                            ESR_EL1_EC_Breakpoint_Lower_EL,
                            ESR_EL1_EC_Breakpoint_Current_EL,
                            ESR_EL1_EC_Software_Step_Exception_Lower_EL,
                            ESR_EL1_EC_Software_Step_Exception_Current_EL,
                            ESR_EL1_EC_Watchpoint_Lower_EL,
                            ESR_EL1_EC_Watchpoint_Current_EL,
                            ESR_EL1_EC_BRK_Instruction_In_Aarch64)
      with Size => 6;

   for ESR_EL1_EC_Type use (ESR_EL1_EC_Unknown => 2#0#,
                            ESR_EL1_EC_Trapped_WFI_WFE => 2#1#,
                            ESR_EL1_EC_Trapped_Access_SME_SVE_Advanced_SIMD_FP => 2#00_0111#,
                            ESR_EL1_EC_Illegal_State => 2#0_1110#,
                            ESR_EL1_EC_Aarch64_SVC => 2#1_0101#,
                            ESR_EL1_EC_Trapped_MSR_MRS_System_Inst_In_AArch64 => 2#1_1000#,
                            ESR_EL1_EC_Instruction_Abort_Lower_EL => 2#10_0000#,
                            ESR_EL1_EC_Instruction_Abort_Current_EL => 2#10_0001#,
                            ESR_EL1_EC_PC_ALignment_Fault => 2#10_0010#,
                            ESR_EL1_EC_Data_Abort_Lower_EL => 2#10_0100#,
                            ESR_EL1_EC_Data_Abort_Current_EL => 2#10_0101#,
                            ESR_EL1_EC_SP_Alignment_Fault => 2#10_0110#,
                            ESR_EL1_EC_Trapped_Floating_Point_Exception => 2#10_1100#,
                            ESR_EL1_EC_SError_Exception => 2#10_1111#,
                            ESR_EL1_EC_Breakpoint_Lower_EL => 2#11_0000#,
                            ESR_EL1_EC_Breakpoint_Current_EL => 2#11_0001#,
                            ESR_EL1_EC_Software_Step_Exception_Lower_EL => 2#11_0010#,
                            ESR_EL1_EC_Software_Step_Exception_Current_EL => 2#11_0011#,
                            ESR_EL1_EC_Watchpoint_Lower_EL => 2#11_0100#,
                            ESR_EL1_EC_Watchpoint_Current_EL => 2#11_0101#,
                            ESR_EL1_EC_BRK_Instruction_In_Aarch64 => 2#11_1100#);

   type ESR_EL1_Type (As_Value : Boolean := True) is record
      case As_Value is
         when True =>
            Value : Interfaces.Unsigned_64 := 0;
         when False =>
            ISS : Bit_Sized_Integer_Types.Twenty_Four_Bits_Type;
            IL : Bit_Sized_Integer_Types.Bit_Type;
            EC : ESR_EL1_EC_Type;
            ISS2 : Bit_Sized_Integer_Types.Twenty_Three_Bits_Type;
      end case;
   end record with
      Size => 64, Bit_Order => System.Low_Order_First, Unchecked_Union;

   for ESR_EL1_Type use record
      Value at 0 range 0 .. 63;
      ISS at 0 range 0 .. 24;
      IL at 0 range 25 .. 25;
      EC at 0 range 26 .. 31;
      ISS2 at 0 range 32 .. 55;
   end record;

   function Get_ESR_EL1 return ESR_EL1_Type
      with Inline_Always;

   function Get_FAR_EL1 return Interfaces.Unsigned_64
      with Inline_Always;

   function Get_ELR_EL1 return Interfaces.Unsigned_64
      with Inline_Always;

   -----------------------------------------------------------------------------
   --  Interrupt Nesting Tracking Data Structures
   -----------------------------------------------------------------------------

   Num_Interrupt_Nesting_Levels : constant := 32;

   --
   --  NOTE: Interrupt nesting level 0 corresponds to thread context.
   --  Interrupt context starts at interrupt nesting level 1.
   --
   type Interrupt_Nesting_Level_Type is range 0 .. Num_Interrupt_Nesting_Levels;

   subtype Valid_Interrupt_Nesting_Level_Type is
      Interrupt_Nesting_Level_Type range 1 .. Num_Interrupt_Nesting_Levels;

   type Interrupt_Nesting_Level_To_Stack_Pointer_Map_Type is
      array (Valid_Interrupt_Nesting_Level_Type) of System.Address;

   type Interrupt_Nesting_Type is record
      --
      --   NOTE: The reset handler is like an interrupt handler (interrupt context).
      --   So the initial interrupt nesting level is 1 not 0.
      --
      Nesting_Level : Interrupt_Nesting_Level_Type := 1;
      Nesting_Level_To_Stack_Pointer_Map : Interrupt_Nesting_Level_To_Stack_Pointer_Map_Type;
   end record with Alignment => Cache_Line_Size_In_Bytes;

   procedure Increase_Interrupt_Nesting
      (Interrupt_Nesting : in out Interrupt_Nesting_Type;
       Stack_Pointer : System.Address)
      with Inline_Always;

   procedure Decrease_Interrupt_Nesting
      (Interrupt_Nesting : in out Interrupt_Nesting_Type)
      with Inline_Always;

   function Get_Interrupt_Nesting_Stack_Pointer (Interrupt_Nesting : Interrupt_Nesting_Type)
      return System.Address is
      (Interrupt_Nesting.Nesting_Level_To_Stack_Pointer_Map (Interrupt_Nesting.Nesting_Level));

   Cpu_To_Interrupt_Nesting :
      array (Valid_Cpu_Core_Id_Type) of Interrupt_Nesting_Type;

   function Get_Cpu_Interrupt_Nesting (Cpu_Id : Valid_Cpu_Core_Id_Type)
      return Interrupt_Nesting_Type is
      (Cpu_To_Interrupt_Nesting (Cpu_Id));

end CPU.Interrupt_Handling;
