--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Utils;
with Board;
with Uart_Boot_Loader_Utils;
with Uart_Boot_Loader_Server;
with GNAT.Source_Info;

procedure App_Main is
begin
   Utils.Print_String (
      ASCII.LF & Board.Board_Name & " UART boot loader server (built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time & ")" & ASCII.LF);

   Uart_Boot_Loader_Server.Load_Image_Over_Uart (Uart_Boot_Loader_Utils.Load_Address);
end App_Main;
