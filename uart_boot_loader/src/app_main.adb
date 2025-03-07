--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Utils;
with Uart_Boot_Loader;
with System;
with GNAT.Source_Info;

procedure App_Main is
   Load_Address : constant System.Address := System'To_Address (16#0010_0000#);
begin
   Utils.Print_String (
      ASCII.LF & "Raspberry PI 4 UART boot loader (built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time & ")" & ASCII.LF);

   Uart_Boot_Loader.Load_Image_Over_Uart (Load_Address);
end App_Main;
