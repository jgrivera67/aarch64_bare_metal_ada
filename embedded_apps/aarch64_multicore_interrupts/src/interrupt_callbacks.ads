--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;
with Interfaces;

package Interrupt_Callbacks is
   use type System.Address;

   procedure Timer_Interrupt_Callback (Arg : System.Address)
      with Pre => Arg /= System.Null_Address;

   procedure Uart_Rx_Interrupt_Callback (Arg : System.Address;
                                         Byte_Received : Interfaces.Unsigned_8)
      with Pre => Arg /= System.Null_Address;
end Interrupt_Callbacks;