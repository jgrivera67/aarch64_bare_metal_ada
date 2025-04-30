--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with System;
with Interfaces;

package Uart_Boot_Loader_Common is
   Load_Address : constant System.Address := System'To_Address (16#0010_0000#);

   type Byte_Array_Type is array (Positive range <>) of Interfaces.Unsigned_8;

   function Compute_Checksum (Bytes_Array : Byte_Array_Type)
      return Interfaces.Unsigned_32;

end Uart_Boot_Loader_Common;
