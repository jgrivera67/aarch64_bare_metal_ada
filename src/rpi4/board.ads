--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  Board-specific declarations for Raspberry PI 4
--
with System;
package Board is
   Board_Name : constant String := "Raspberry PI 4";

   --
   --  Main peripherals and ARM local peripherals in "low peripheral" mode (see section 1.2,
   --  figure 1 of bcm2711-peripherals.pdf)
   --
   Global_Mmio_Region_Start_Address : constant System.Address := System'To_Address (16#fc00_0000#);
   Global_Mmio_Region_End_Address : constant System.Address := System'To_Address (16#1_0000_0000#);

   UART0_Base : constant System.Address := System'To_Address (16#fe20_1000#);

   GICD_Base_Address : constant System.Address := System'To_Address (16#ff84_1000#);
   GICC_Base_Address : constant System.Address := System'To_Address (16#ff84_2000#);
end Board;