--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--
with CPU;
with Utils;

package body Interrupt_Callbacks is
   use type Interfaces.Unsigned_32;

   procedure Timer_Interrupt_Callback (Arg : System.Address) is
      Counter : Interfaces.Unsigned_32 with Address => Arg, Import;
   begin
      Counter := @ + 1;
      Utils.Print_String ("*** Timer interrupt has fired ");
      Utils.Print_Number_Decimal (Counter);
      Utils.Print_String (" times" & ASCII.LF);
   end Timer_Interrupt_Callback;

   procedure Uart_Rx_Interrupt_Callback (Arg : System.Address;
                                         Byte_Received : Interfaces.Unsigned_8) is
      Counter : Interfaces.Unsigned_32 with Address => Arg, Import;
   begin
      Counter := @ + 1;
      Utils.Print_String ("*** UART interrupt has fired ");
      Utils.Print_Number_Decimal (Counter);
      Utils.Print_String (" times (byte received: ");
      Utils.Put_Char (Character'Val (Byte_Received));
      Utils.Print_String (")" & ASCII.LF);

      if Character'Val (Byte_Received) = Utils.Ctrl_C then
         --  Enter the self-hosted debugger:
         Utils.Print_String (ASCII.LF & "^C" & ASCII.LF);
         CPU.Break_Point;
      end if;
   end Uart_Rx_Interrupt_Callback;

end Interrupt_Callbacks;