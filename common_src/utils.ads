--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with Uart_Driver;
with System;
with Interfaces;

package Utils is
   use type Interfaces.Unsigned_64;

   Instruction_Size_In_Bytes : constant := 4;

   Cache_Line_Size_In_Bytes : constant := 64;

   Page_Size_In_Bytes : constant := 4_096;

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

   function Get_ELR_EL1 return Interfaces.Unsigned_64;

   procedure Wait_For_Interrupt with Inline_Always;

   procedure Break_Point with Inline_Always;

   procedure Park_Cpu with
      Import,
      External_Name => "park_cpu", --  defined in utils_asm.S
      No_Return;

private

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with No_Return,
          Export,
          Convention => C,
          External_Name => "__gnat_last_chance_handler";

   function Get_Call_Address return System.Address with
      Inline_Always => False, Suppress => All_Checks;

   function Get_Code_Location_Here return System.Address is
      (Get_Call_Address);

end Utils;
