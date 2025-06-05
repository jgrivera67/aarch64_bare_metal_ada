--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interrupt_Callbacks;
with Board;
with CPU.Memory_Protection;
with CPU.Multicore;
with CPU.Interrupt_Handling;
with Timer_Driver;
with Uart_Driver;
with Utils;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   use type CPU.Valid_Cpu_Core_Id_Type;
   Code_Address : constant System.Address := CPU.Get_Reset_Handler_Address;
   Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
   Timer_Interrupts_Counter : Interfaces.Unsigned_32 := 0;
   Uart_Interrupts_Counter : Interfaces.Unsigned_32 := 0;
begin
   if Cpu_Id = CPU.Valid_Cpu_Core_Id_Type'First then
      Utils.Print_String (
         Board.Board_Name & " AArch64 Multicore Interrupts - built on " &
         GNAT.Source_Info.Compilation_Date &
         " at " & GNAT.Source_Info.Compilation_Time &
         ", boot address ");
      Utils.Print_Number_Hexadecimal (
         Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
         End_Line => True);

      --
      --  NOTE: For Raspberry PI 4, we need to start secondary cores before calling
      --  `CPU.Memory_Protection.Initialize`, as the spin tables are in page 0, which
      --  cannot be written after enabling the MMU.
      --
      --  For CPU 0, App_Main is invoked from the the GNAT-generated main
      --  after doing Ada package elaboration. So, at this point, we can
      --  release secondary cores.
      --
      CPU.Multicore.Start_Secondary_Cpus;
   end if;

   CPU.Memory_Protection.Initialize;
   Timer_Driver.Initialize;
   CPU.Interrupt_Handling.Initialize;

   if Cpu_Id = CPU.Valid_Cpu_Core_Id_Type'First then
      Utils.Lock_Console (Print_Cpu => False);
      Uart_Driver.Flush_Output;
      Uart_Driver.Initialize_Uart (Interrupt_Callbacks.Uart_Rx_Interrupt_Callback'Access,
                                   Rx_Interrupt_Callback_Arg => Uart_Interrupts_Counter'Address);
      Utils.Unlock_Console;
   end if;

   Timer_Driver.Start_Timer (Interrupt_Callbacks.Timer_Interrupt_Callback'Access,
                             Timer_Interrupt_Callback_Arg => Timer_Interrupts_Counter'Address,
                             Expiration_Delta_Time_Us => 1_000_000);

   loop
      CPU.Interrupt_Handling.Wait_For_Interrupt;
   end loop;
end App_Main;
