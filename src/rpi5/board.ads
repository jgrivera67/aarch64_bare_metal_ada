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
with CPU.Multicore;

package Board is
   Board_Name : constant String := "Raspberry PI 5";

   --
   --  Main peripherals and ARM local peripherals in "high peripheral" mode.
   --

   --  16#10_7000_0000# rounded-down to previous 1GB boundary
   Global_Mmio_Region_Start_Address : constant System.Address := System'To_Address (16#10_4000_0000#);

   --  16#10_f000_0000# rounded-up to next 1GB boundary
   Global_Mmio_Region_End_Address : constant System.Address := System'To_Address (16#10_8000_0000#);

   UART0_Base : constant System.Address := System'To_Address (16#10_7d00_1000#);

   GICD_Base_Address : constant System.Address := System'To_Address (16#10_7fff_9000#);
   GICC_Base_Address : constant System.Address := System'To_Address (16#10_7fff_a000#);

   use CPU.Multicore;

   procedure Start_Secondary_Cpu (Cpu_Id : Secondary_Cpu_Core_Id_Type;
                                  Entry_Point_Address : System.Address)
      with Pre => CPU.Cpu_In_Privileged_Mode and then
                  Get_Cpu_Id = Valid_Cpu_Core_Id_Type'First;
end Board;