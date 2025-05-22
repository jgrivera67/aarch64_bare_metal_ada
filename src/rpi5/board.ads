--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  Board-specific interface for Raspberry PI 5
--
with CPU.Multicore;
with System.Storage_Elements;

package Board is
   Board_Name : constant String := "Raspberry PI 5";

   Cpu_Model : constant CPU.Cpu_Model_Type := CPU.Cortex_A76;

   --
   --  Main peripherals and ARM local peripherals in "high peripheral" mode.
   --

   --  16#10_7000_0000# rounded-down to previous 1GB boundary
   Global_Mmio_Region_Start_Address : constant System.Address := System'To_Address (16#10_4000_0000#);

   --  16#10_f000_0000# rounded-up to next 1GB boundary
   Global_Mmio_Region_End_Address : constant System.Address := System'To_Address (16#10_8000_0000#);

   UART0_Base : constant System.Address := System'To_Address (16#10_7d00_1000#);

   Uart_Clock_Frequency_Hz : constant := 44_236_800;

   GICD_Base_Address : constant System.Address := System'To_Address (16#10_7fff_9000#);
   GICC_Base_Address : constant System.Address := System'To_Address (16#10_7fff_a000#);

   use type CPU.Cpu_Core_Id_Type;
   use type System.Storage_Elements.Integer_Address;
   procedure Start_Secondary_Cpu (Cpu_Id : CPU.Secondary_Cpu_Core_Id_Type;
                                  Entry_Point_Address : System.Address)
      with Pre => CPU.Cpu_In_Privileged_Mode and then
                  CPU.Multicore.Get_Cpu_Id = CPU.Valid_Cpu_Core_Id_Type'First and then
                  System.Storage_Elements.To_Integer (Entry_Point_Address) mod CPU.Instruction_Size_In_Bytes = 0;
end Board;