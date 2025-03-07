--
--  Copyright (c) 2022-2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Uart_Boot_Loader;
with System;

procedure App_Main is
   Load_Address : constant System.Address := System'To_Address (16#0000_0000#);
begin
   Uart_Boot_Loader.Load_Image_Over_Uart (Load_Address);
   Uart_Boot_Loader.Jump_To_Image_Reset_Handler (Load_Address);
end App_Main;
