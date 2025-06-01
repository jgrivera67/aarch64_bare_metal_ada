--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with CPU;
with Timer_Driver;
with Uart_Driver;
private with CPU.Multicore;
with System.Storage_Elements;
with Interfaces;

package Utils is

   procedure Put_Char (C : Character);

   procedure Print_String (S : String; End_Line : Boolean := False);

   procedure Print_Number_Decimal (Value : Interfaces.Unsigned_32;
                                   End_Line : Boolean := False);

   procedure Print_Number_Hexadecimal (Value : Interfaces.Unsigned_64;
                                       End_Line : Boolean := False);

   procedure Print_Number_Hexadecimal (Value : Interfaces.Unsigned_8;
                                       End_Line : Boolean := False);

   function Receive_Byte_With_Timeout (Timeout_Us : Timer_Driver.Delta_Time_Us_Type)
      return Uart_Driver.Maybe_Byte_Type;

   function Get_Char return Character renames Uart_Driver.Get_Char;

   procedure Wait_For_Ctrl_C
      with Pre => CPU.Cpu_Interrupting_Disabled;

   procedure Lock_Console (Print_Cpu : Boolean := True);

   procedure Unlock_Console;

   function Console_Lock_Owner return CPU.Cpu_Core_Id_Type;

   function Get_PC_Here return System.Address
      with Inline_Always => False;

   procedure System_Crash
      with No_Return,
           Pre => CPU.Cpu_In_Privileged_Mode;

   procedure Copy_String (Dest : out String;
                          Source : String;
                          Cursor_Index : in out Positive)
      with Pre => Cursor_Index in Dest'Range,
           Post => Cursor_Index = Cursor_Index'Old + Source'Length and then
                   Cursor_Index in Dest'Range;

   function Equal_Strings (Str1, Str2 : String) return Boolean;

   type Byte_Array_Type is
      array (System.Storage_Elements.Integer_Address range <>) of Interfaces.Unsigned_8;

   Ctrl_C : constant Character := ASCII.ETX;

private

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with No_Return,
          Export,
          Convention => C,
          External_Name => "__gnat_last_chance_handler";

   Console_Spinlock : CPU.Multicore.Spinlock_Type;

   function Console_Lock_Owner return CPU.Cpu_Core_Id_Type is
      (CPU.Multicore.Spinlock_Owner (Console_Spinlock));
end Utils;
