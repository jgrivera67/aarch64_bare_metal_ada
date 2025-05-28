--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with Uart_Driver;
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

   function Receive_Byte_With_Timeout (Timeout_Usec : Interfaces.Unsigned_64)
      return Uart_Driver.Maybe_Byte_Type;

   function Get_Char return Character renames Uart_Driver.Get_Char;

   procedure Lock_Console (Print_Cpu : Boolean);

   procedure Unlock_Console;

   procedure Copy_String (Dest : out String;
                          Source : String;
                          Cursor_Index : in out Positive)
      with Pre => Cursor_Index in Dest'Range,
           Post => Cursor_Index = Cursor_Index'Old + Source'Length and then
                   Cursor_Index in Dest'Range;

   type Byte_Array_Type is
      array (System.Storage_Elements.Integer_Address range <>) of Interfaces.Unsigned_8;

private

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with No_Return,
          Export,
          Convention => C,
          External_Name => "__gnat_last_chance_handler";

end Utils;
