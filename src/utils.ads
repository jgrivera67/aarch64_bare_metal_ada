--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

private with CPU;
with Uart_Driver;
with System;
with Interfaces;

package Utils is

   procedure Put_Char (C : Character);

   procedure Print_String (S : String; End_Line : Boolean := False);

   procedure Print_Number_Decimal (Value : Interfaces.Unsigned_32;
                                   End_Line : Boolean := False);

   procedure Print_Number_Hexadecimal (Value : Interfaces.Unsigned_64;
                                       End_Line : Boolean := False);

   function Receive_Byte_With_Timeout (Timeout_Usec : Interfaces.Unsigned_64)
      return Uart_Driver.Maybe_Byte_Type;

   function Get_Char return Character renames Uart_Driver.Get_Char;

   function Get_Code_Location_Here return System.Address
      with Inline_Always;

private

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with No_Return,
          Export,
          Convention => C,
          External_Name => "__gnat_last_chance_handler";

   function Get_Code_Location_Here return System.Address is
      (CPU.Get_Call_Address);

end Utils;
