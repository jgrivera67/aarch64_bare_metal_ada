--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with CPU;
with Utils;
with Uart_Boot_Loader_Utils;
with Uart_Boot_Loader_Server;
with System.Storage_Elements;
with Interfaces;
with GNAT.Source_Info;

procedure App_Main is
   use System.Storage_Elements;
   Code_Address : constant System.Address := CPU.Get_Reset_Handler_Address;
begin
   Utils.Print_String (
      ASCII.LF & Board.Board_Name & " UART boot loader server - built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time & ", boot address ");
      Utils.Print_Number_Hexadecimal (
         Interfaces.Unsigned_64 (To_Integer (Code_Address)), End_Line => True);

   Uart_Boot_Loader_Server.Load_Image_Over_Uart (Uart_Boot_Loader_Utils.Load_Address);
end App_Main;
