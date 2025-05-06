--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with CPU;
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

   -----------------------------------------------------------------------------
   --  Bit manipulation utilities
   -----------------------------------------------------------------------------

   use CPU;

   type Bit_Index_Type is mod Cpu_Register_Type'Size;

   function Bit_Mask (Bit_Index : Bit_Index_Type) return Cpu_Register_Type is
    (Cpu_Register_Type (2 ** Natural (Bit_Index)));

   function Is_Value_Power_Of_Two (Value : Cpu_Register_Type) return Boolean is
      (Value /= 0 and then
       (Value and (Value - 1)) = 0);

   subtype Log_Base_2_Type is Natural range 0 .. Natural (Bit_Index_Type'Last);

   function Get_Log_Base_2 (Value : Cpu_Register_Type) return Log_Base_2_Type
      with Pre => Is_Value_Power_Of_Two (Value) and then
                  Value <= Cpu_Register_Type'Last;

private

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with No_Return,
          Export,
          Convention => C,
          External_Name => "__gnat_last_chance_handler";

   function Get_Code_Location_Here return System.Address is
      (CPU.Get_Call_Address);

   function Get_Log_Base_2 (Value : Cpu_Register_Type) return Log_Base_2_Type is
      (Log_Base_2_Type'Last - Log_Base_2_Type (CPU.Count_Leading_Zeros (Value)));

end Utils;
