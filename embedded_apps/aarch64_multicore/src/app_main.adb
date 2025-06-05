--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with CPU.Memory_Protection;
with CPU.Multicore;
with Timer_Driver;
with Utils;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   use type CPU.Valid_Cpu_Core_Id_Type;
   use type Interfaces.Unsigned_32;

   procedure Busy_Wait (Delay_Us : Timer_Driver.Delta_Time_Us_Type) is
      use type Timer_Driver.Timestamp_Us_Type;
      use type Timer_Driver.Delta_Time_Us_Type;
      Start_Timestamp_Us : constant Timer_Driver.Timestamp_Us_Type :=
         Timer_Driver.Get_Timestamp_Us;
      End_Timestamp_Us : Timer_Driver.Timestamp_Us_Type;
   begin
      loop
         End_Timestamp_Us := Timer_Driver.Get_Timestamp_Us;
         exit when Timer_Driver.Delta_Time_Us_Type (
            End_Timestamp_Us - Start_Timestamp_Us) >= Delay_Us;
      end loop;
   end Busy_Wait;

   Code_Address : constant System.Address := CPU.Get_Reset_Handler_Address;
   Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
   Count : Interfaces.Unsigned_32 := 0;
begin
   if Cpu_Id = CPU.Valid_Cpu_Core_Id_Type'First then
      Utils.Print_String (
         Board.Board_Name & " AArch64 Multicore - built on " &
         GNAT.Source_Info.Compilation_Date &
         " at " & GNAT.Source_Info.Compilation_Time &
         ", boot address ");
      Utils.Print_Number_Hexadecimal (
         Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
         End_Line => True);

      --
      --  NOTE: For CPU 0, App_Main is invoked from the the GNAT-generated main
      --  after doing Ada package elaboration. So, ath this point, we can
      --  release secondary cores.
      --
      CPU.Multicore.Start_Secondary_Cpus;
   end if;

   CPU.Memory_Protection.Initialize;
   loop
      Utils.Lock_Console;
      Utils.Print_String ("Running iteration ");
      Utils.Print_Number_Decimal (Count, End_Line => True);
      Utils.Unlock_Console;
      Busy_Wait (1_000_000);
      Count := @ + 1;
   end loop;
end App_Main;
