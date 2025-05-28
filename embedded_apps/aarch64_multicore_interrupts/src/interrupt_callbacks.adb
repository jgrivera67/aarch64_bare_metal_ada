--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--
with Utils;

package body Interrupt_Callbacks is
   use type Interfaces.Unsigned_32;

   procedure Timer_Interrupt_Callback (Arg : System.Address) is
      Counter : Interfaces.Unsigned_32 with Address => Arg, Import;
   begin
      Counter := @ + 1;
      Utils.Lock_Console (Print_Cpu => True);
      Utils.Print_String ("*** Timer interrupt has fired ");
      Utils.Print_Number_Decimal (Counter);
      Utils.Print_String (" times" & ASCII.LF);
      Utils.Unlock_Console;
   end Timer_Interrupt_Callback;

   procedure Uart_Rx_Interrupt_Callback (Arg : System.Address;
                                         Byte_Received : Interfaces.Unsigned_8) is
      Counter : Interfaces.Unsigned_32 with Address => Arg, Import;
   begin
      Counter := @ + 1;
      Utils.Lock_Console (Print_Cpu => True);
      Utils.Print_String ("*** UART interrupt has fired ");
      Utils.Print_Number_Decimal (Counter);
      Utils.Print_String (" times (byte received: ");
      if Character'Val (Byte_Received) in ' ' .. '~' then
         Utils.Put_Char (''');
         Utils.Put_Char (Character'Val (Byte_Received));
         Utils.Put_Char (''');
      else
         Utils.Print_String ("ASCII code ");
         Utils.Print_Number_Hexadecimal (Byte_Received);
      end if;
      Utils.Print_String (")" & ASCII.LF);
      Utils.Unlock_Console;
   end Uart_Rx_Interrupt_Callback;

end Interrupt_Callbacks;