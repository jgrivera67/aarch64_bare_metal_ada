--
--  Copyright (c) 2024, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with System;
with Interfaces;
private with Uart_Driver;

package Uart_Boot_Loader is
   procedure Load_Image_Over_Uart (Load_Address : System.Address);

   procedure Jump_To_Image_Reset_Handler (Reset_Handler_Address : System.Address);

private
   use type Interfaces.Unsigned_64;

   procedure Print_String (S : String; End_Line : Boolean := False);

   procedure Print_Number_Decimal (Value : Interfaces.Unsigned_32;
                                   End_Line : Boolean := False);

   function Receive_Byte_With_Timeout (Timeout_Usec : Interfaces.Unsigned_64)
      return Uart_Driver.Maybe_Byte_Type;

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
      (Get_CNTPCT  / (Get_CNTFRQ / 1_000_000));

end Uart_Boot_Loader;
