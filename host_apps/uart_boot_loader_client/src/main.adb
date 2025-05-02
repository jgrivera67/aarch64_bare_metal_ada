--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Uart_Boot_Loader_Client;
with Ada.Command_Line;
with Ada.Text_IO;

procedure Main is
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         "Usage: uart_boot_loader_client <.bin file> > /dev/ttyXX < /dev/ttyXX");
      raise Program_Error;
   end if;

   Uart_Boot_Loader_Client.Send_File (Ada.Command_Line.Argument (1));

exception
   when others =>
      Ada.Command_Line.Set_Exit_Status (1);
end Main;
