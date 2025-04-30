--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with System;
with Interfaces;

package Uart_Boot_Loader_Server is
   procedure Load_Image_Over_Uart (Load_Address : System.Address);

private
   Receive_Timeout_Usec : constant Interfaces.Unsigned_64 := 1_000_000;  --  1s

   type Maybe_Checksum_Type is record
      Valid : Boolean := False;
      Checksum : Interfaces.Unsigned_32;
   end record;

   procedure Receive_File (Load_Address : System.Address);

   procedure Jump_To_Image_Reset_Handler (Reset_Handler_Address : System.Address);

end Uart_Boot_Loader_Server;
