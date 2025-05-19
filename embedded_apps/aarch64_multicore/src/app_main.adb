--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with CPU.Memory_Protection;
with CPU.Multicore;
with Utils;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   use type CPU.Valid_Cpu_Core_Id_Type;
   Code_Address : constant System.Address := CPU.Get_Reset_Handler_Address;
   Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
begin
   if Cpu_Id = CPU.Valid_Cpu_Core_Id_Type'First then
      --
      --  NOTE: For CPU 0, App_Main is invoked from the the GNAT-generated main
      --  after doing Ada package elaboration. So, ath this point, we can
      --  release secondary cores.
      --
      CPU.Multicore.Start_Secondary_Cpus;
   end if;

   CPU.Memory_Protection.Configure_Global_Regions;
   loop
      Utils.Lock_Console (Print_Cpu => True);
      Utils.Print_String (
         Board.Board_Name & " AArch64 Multicore - built on " &
         GNAT.Source_Info.Compilation_Date &
         " at " & GNAT.Source_Info.Compilation_Time &
         ", boot address ");
      Utils.Print_Number_Hexadecimal (
         Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
         End_Line => True);
      Utils.Unlock_Console;
   end loop;
end App_Main;
