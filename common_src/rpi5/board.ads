--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  Board-specific declarations for Raspberry PI 5
--
with System;
package Board is
   Board_Name : constant String := "Raspberry PI 5";

   --
   --  Main peripherals and ARM local peripherals in "high peripheral" mode.
   --
   Global_Mmio_Region_Start_Address : constant System.Address := System'To_Address (16#10_7000_0000#);
   Global_Mmio_Region_End_Address : constant System.Address := System'To_Address (16#10_f000_0000#);

   UART0_Base : constant System.Address := System'To_Address (16#10_7d00_1000#);

   GICD_Base_Address : constant System.Address := System'To_Address (16#10_7fff_9000#);
   GICC_Base_Address : constant System.Address := System'To_Address (16#10_7fff_a000#);
end Board;