--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Uart_Boot_Loader_Utils is
   function Compute_Checksum (Bytes_Array : Byte_Array_Type)
      return Interfaces.Unsigned_32
   is
      use type Interfaces.Unsigned_32;
      CRC_32_Polynomial : constant := 16#04c11db7#;
      CRC : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;
      Data_Byte : Interfaces.Unsigned_8;
   begin
      for B of Bytes_Array loop
         Data_Byte := B;
         for I in 1 .. 8 loop
            if ((Interfaces.Unsigned_32 (Data_Byte) xor CRC) and 1) /= 0 then
               CRC := Interfaces.Shift_Right (CRC, 1);
               Data_Byte := Interfaces.Shift_Right (Data_Byte, 1);
               CRC := CRC xor CRC_32_Polynomial;
            else
               CRC := Interfaces.Shift_Right (CRC, 1);
               Data_Byte := Interfaces.Shift_Right (Data_Byte, 1);
            end if;
         end loop;
      end loop;

      return CRC;
   end Compute_Checksum;
end Uart_Boot_Loader_Utils;
