--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  Self-hosted debugger GDB server
--
with CPU.Interrupt_Handling;
with CPU.Self_Hosted_Debug;
with Utils;
with System;
private with System.Storage_Elements;
with Interfaces;

package Gdb_Server with SPARK_Mode => On is
   procedure Run_Gdb_Server (
      Debug_Event : CPU.Self_Hosted_Debug.Debug_Event_Type;
      Current_PC : in out System.Address)
      with Pre => CPU.Cpu_In_Privileged_Mode and then
                  CPU.Cpu_Interrupting_Disabled;

private
   use CPU.Self_Hosted_Debug;
   use System.Storage_Elements;

   type Hardware_Breakpoint_Array_Type is
      array (Valid_Hardware_Breakpoint_Id_Type) of Hardware_Breakpoint_Type;

   type Watchpoint_Array_Type is
      array (Valid_Watchpoint_Id_Type) of Watchpoint_Type;

   Max_Gdb_Packet_Payload_Size_In_Bytes : constant := 1024;

   Trap_Signal : constant := 16#5#;

   Isr_Fake_Thread_Id : constant := Integer_Address'Last;

   type Gdb_Packet_Data_Length_Type is range 0 .. Max_Gdb_Packet_Payload_Size_In_Bytes;

   subtype Gdb_Packet_Data_Index_Type is
      Positive range 1 .. Max_Gdb_Packet_Payload_Size_In_Bytes + 1;

   subtype Valid_Gdb_Packet_Data_Index_Type is Gdb_Packet_Data_Index_Type range
      Gdb_Packet_Data_Index_Type'First .. Gdb_Packet_Data_Index_Type'Last - 1;

   type Gdb_Server_Type is limited record
      --  Flag indicating that a GDB client is currently attached
      Gdb_Attached : Boolean := False;

      --  Flag indicating that the target being debugged must be resumed
      Resume_Target : Boolean := False;

      --  PC where the exception that caused the self-hosted debugger to be entered happened
      Current_PC : System.Address := System.Null_Address;

      --  Self-hosted debug event that caused the self-hosted debugger to be entered
      Enter_Debug_Event : Debug_Event_Type;

      --  Number of hardware breakpoints currently in use
      Hardware_Breakpoints_In_Use : Num_Hardware_Breakpoints_Type := 0;

      --  Number of hardware watchpoints currently in use
      Watchpoints_In_Use : Num_Watchpoints_Type := 0;

      --  State fo each hardware breakpoint
      Hardware_Breakpoints : Hardware_Breakpoint_Array_Type;

      --  State fo each watchpoint
      Watchpoints : Watchpoint_Array_Type;

      --  Checksum being computed for the current GDB packet being sent, which could
      --  consists of multiple packet fragments.
      Computed_Checksum : Interfaces.Unsigned_8 := 0;

      --  Payload length (in bytes) of the last GDB packet sent/received
      Gdb_Packet_Data_Length : Gdb_Packet_Data_Length_Type;

      --  Buffer for the payload of the last Last GDB packet sent or received
      Gdb_Packet_Data_Buffer : String (Valid_Gdb_Packet_Data_Index_Type);

      --  Last thread Id retrieved to respond to the last qfThreadInfo/qsThreadInfo
      --  GDB packet received
      Last_Thread_Id : Integer_Address := To_Integer (System.Null_Address);

      --  Current thread Id set by the last `Hg<thread id>` packet received.
      Current_Hg_Thread_Id : Integer_Address := To_Integer (System.Null_Address);

      Self_Hosted_Debug_Capabilities : CPU.Self_Hosted_Debug.Capabilities_Type;
   end record;

   Gdb_Server_Objects : array (CPU.Valid_Cpu_Core_Id_Type) of Gdb_Server_Type;

   type Gdb_Cpu_Register_Id_Type is (Gdb_X0, Gdb_X1,
                                     Gdb_X2, Gdb_X3,
                                     Gdb_X4, Gdb_X5,
                                     Gdb_X6, Gdb_X7,
                                     Gdb_X8, Gdb_X9,
                                     Gdb_X10, Gdb_X11,
                                     Gdb_X12, Gdb_X13,
                                     Gdb_X14, Gdb_X15,
                                     Gdb_X16, Gdb_X17,
                                     Gdb_X18, Gdb_X19,
                                     Gdb_X20, Gdb_X21,
                                     Gdb_X22, Gdb_X23,
                                     Gdb_X24, Gdb_X25,
                                     Gdb_X26, Gdb_X27,
                                     Gdb_X28, Gdb_X29,
                                     Gdb_X30);

   Gdb_Cpu_Register_Id_To_Cpu_Register_Id : constant
      array (Gdb_Cpu_Register_Id_Type) of CPU.Interrupt_Handling.Cpu_Register_Id_Type := [
      Gdb_X0 => CPU.Interrupt_Handling.X0,
      Gdb_X1 => CPU.Interrupt_Handling.X1,
      Gdb_X2 => CPU.Interrupt_Handling.X2,
      Gdb_X3 => CPU.Interrupt_Handling.X3,
      Gdb_X4 => CPU.Interrupt_Handling.X4,
      Gdb_X5 => CPU.Interrupt_Handling.X5,
      Gdb_X6 => CPU.Interrupt_Handling.X6,
      Gdb_X7 => CPU.Interrupt_Handling.X7,
      Gdb_X8 => CPU.Interrupt_Handling.X8,
      Gdb_X9 => CPU.Interrupt_Handling.X9,
      Gdb_X10 => CPU.Interrupt_Handling.X10,
      Gdb_X11 => CPU.Interrupt_Handling.X11,
      Gdb_X12 => CPU.Interrupt_Handling.X12,
      Gdb_X13 => CPU.Interrupt_Handling.X13,
      Gdb_X14 => CPU.Interrupt_Handling.X14,
      Gdb_X15 => CPU.Interrupt_Handling.X15,
      Gdb_X16 => CPU.Interrupt_Handling.X16,
      Gdb_X17 => CPU.Interrupt_Handling.X17,
      Gdb_X18 => CPU.Interrupt_Handling.X18,
      Gdb_X19 => CPU.Interrupt_Handling.X19,
      Gdb_X20 => CPU.Interrupt_Handling.X20,
      Gdb_X21 => CPU.Interrupt_Handling.X21,
      Gdb_X22 => CPU.Interrupt_Handling.X22,
      Gdb_X23 => CPU.Interrupt_Handling.X23,
      Gdb_X24 => CPU.Interrupt_Handling.X24,
      Gdb_X25 => CPU.Interrupt_Handling.X25,
      Gdb_X26 => CPU.Interrupt_Handling.X26,
      Gdb_X27 => CPU.Interrupt_Handling.X27,
      Gdb_X28 => CPU.Interrupt_Handling.X28,
      Gdb_X29 => CPU.Interrupt_Handling.X29_Or_FP,
      Gdb_X30 => CPU.Interrupt_Handling.X30_Or_LR
   ];

   procedure Receive_Gdb_Packet (Gdb_Server_Obj : in out Gdb_Server_Type)
      with Post => Gdb_Server_Obj.Gdb_Packet_Data_Length /= 0;

   procedure Send_Gdb_Packet (Gdb_Server_Obj : in out Gdb_Server_Type);

   procedure Send_Gdb_Packet_Fragment (Gdb_Server_Obj : in out Gdb_Server_Type;
                                       Is_Packet_First_Fragment : Boolean;
                                       Is_Packet_Last_Fragment : Boolean);

   procedure Print_Gdb_Packet_Data (Gdb_Server_Obj : Gdb_Server_Type;
                                    Label : String);

   procedure Send_Gdb_Stop_Reply_Packet (Gdb_Server_Obj : in out Gdb_Server_Type);

   procedure Send_Gdb_Data_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                   Data : String);

   procedure Send_Gdb_Error_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                    Error_Str : String);

   procedure Send_Gdb_Ok_Packet (Gdb_Server_Obj : in out Gdb_Server_Type);

   procedure Send_Uppercase_S_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                      Signal_Num : Interfaces.Unsigned_8);

   procedure Send_Uppercase_T_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                      Signal_Num : Interfaces.Unsigned_8;
                                      Stop_Reason : String);

   procedure Process_Incoming_Gdb_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                          Packet_Command : Character;
                                          Packet_Args : String);

   procedure Process_Uppercase_D_Packet (Gdb_Server_Obj : in out Gdb_Server_Type);

   procedure Process_Lowercase_C_Packet (Gdb_Server_Obj : in out Gdb_Server_Type);

   procedure Process_Lowercase_G_Packet (Gdb_Server_Obj : in out Gdb_Server_Type)
      with Pre => CPU.Interrupt_Handling.Cpu_In_Interrupt_Context;

   procedure Process_Uppercase_H_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Lowercase_K_Packet (Gdb_Server_Obj : in out Gdb_Server_Type);

   procedure Send_One_Memory_Fragment (Gdb_Server_Obj : in out Gdb_Server_Type;
                                       Src_Data_Buffer : Utils.Byte_Array_Type;
                                       Is_Packet_First_Fragment : Boolean;
                                       Is_Packet_Last_Fragment : Boolean)
      with Pre => Src_Data_Buffer'Length /= 0 and then
                  2 * Src_Data_Buffer'Length <= Max_Gdb_Packet_Payload_Size_In_Bytes;

   procedure Parse_Packet_Arg (Gdb_Server_Obj : in out Gdb_Server_Type;
                               Packet_Args : String;
                               Parsing_Cursor : in out Gdb_Packet_Data_Index_Type;
                               Arg_Separator : Character;
                               Arg_Value : out Integer_Address;
                               Parsing_Ok : out Boolean)
   with Pre => Packet_Args'Length > 0 and then
               Parsing_Cursor in Packet_Args'Range;

   procedure Process_Lowercase_M_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Uppercase_M_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Lowercase_P_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String)
      with Pre => CPU.Interrupt_Handling.Cpu_In_Interrupt_Context;

   procedure Process_Lowercase_Q_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Uppercase_Q_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Uppercase_T_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Lowercase_V_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   use type System.Address;

   procedure Allocate_Hardware_Breakpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                           Target_Addr : System.Address;
                                           Hardware_Breakpoint_Id : out Hardware_Breakpoint_Id_Type)
      with Pre => Target_Addr /= System.Null_Address;

   procedure Free_Hardware_Breakpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                       Target_Addr : System.Address;
                                       Hardware_Breakpoint_Id : out Hardware_Breakpoint_Id_Type)
      with Pre => Target_Addr /= System.Null_Address;

   procedure Allocate_Watchpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                  Target_Addr : System.Address;
                                  Watchpoint_Id : out Watchpoint_Id_Type)
      with Pre => Target_Addr /= System.Null_Address;

   procedure Free_Watchpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                              Target_Addr : System.Address;
                              Watchpoint_Id : out Watchpoint_Id_Type)
      with Pre => Target_Addr /= System.Null_Address;

   procedure Configure_Hardware_Stop_Point (Gdb_Server_Obj : in out Gdb_Server_Type;
                                            Packet_Args : String;
                                            Enable_Stop_Point : Boolean);

   procedure Process_Uppercase_X_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Uppercase_Z_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Lowercase_Z_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String);

   procedure Process_Question_Mark_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                           Packet_Args : String);

end Gdb_Server;
