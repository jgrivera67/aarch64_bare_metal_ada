--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  Board-specific interface for Raspberry PI 4
--
with CPU.Multicore;
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

   use CPU.Multicore;

   procedure Start_Secondary_Cpu (Cpu_Id : Secondary_Cpu_Core_Id_Type;
                                  Entry_Point_Address : System.Address)
      with Pre => CPU.Cpu_In_Privileged_Mode and then
                  Get_Cpu_Id = Valid_Cpu_Core_Id_Type'First;

private
   -----------------------------------------------------------------------------
   --  Secondary CPU cores declarations
   -----------------------------------------------------------------------------

   Secondary_Cpu_To_Spin_Address_Map :
      constant array (Secondary_Cpu_Core_Id_Type) of System.Address :=
      [
         1 => System'To_Address (16#e0#),
         2 => System'To_Address (16#e8#),
         3 => System'To_Address (16#f0#)
      ];
end Board;