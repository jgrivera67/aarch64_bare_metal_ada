--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with System;
with Interfaces;
with Uart_Driver;

package Utils is
   use type Interfaces.Unsigned_64;

   type Byte_Array_Type is array (Positive range <>) of Interfaces.Unsigned_8;

   procedure Print_String (S : String; End_Line : Boolean := False);

   procedure Print_Number_Decimal (Value : Interfaces.Unsigned_32;
                                   End_Line : Boolean := False);

   procedure Print_Number_Hexadecimal (Value : Interfaces.Unsigned_64;
                                       End_Line : Boolean := False);

   function Receive_Byte_With_Timeout (Timeout_Usec : Interfaces.Unsigned_64)
      return Uart_Driver.Maybe_Byte_Type;

   function Compute_Checksum (Bytes_Array : Byte_Array_Type)
      return Interfaces.Unsigned_32;

   function Get_Code_Location_Here return System.Address
      with Inline_Always;

private

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with No_Return,
          Export,
          Convention => C,
          External_Name => "__gnat_last_chance_handler";

   function Get_CNTPCT return Interfaces.Unsigned_64
      with Inline_Always;

   function Get_CNTFRQ return Interfaces.Unsigned_64
      with Inline_Always;

   function Get_Timer_Timestamp_Usec return Interfaces.Unsigned_64 is
      (Get_CNTPCT / (Get_CNTFRQ / 1_000_000));

   function Get_Call_Address return System.Address with
      Inline_Always => False, Suppress => All_Checks;

   function Get_Code_Location_Here return System.Address is
      (Get_Call_Address);

end Utils;
