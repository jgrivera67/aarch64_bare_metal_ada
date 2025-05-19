--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with CPU;
with CPU.Memory_Protection;
with Utils;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   use ASCII;
   Code_Address : constant System.Address := CPU.Get_Reset_Handler_Address;
   C : Character;

   procedure Cause_Data_Abort_Unmapped_Write is
      Invalid_Address : constant System.Address := System.Storage_Elements.To_Address (16#ff00_0000_0000#);
      X : Interfaces.Unsigned_8 with Address => Invalid_Address, Import;
   begin
      Utils.Print_String (LF & "Causing data abort for unmapped address write ..." & LF);
      X := 1;
   end Cause_Data_Abort_Unmapped_Write;

   procedure Cause_Data_Abort_Null_Pointer_Write is
      X : Interfaces.Unsigned_8 with Address => System.Null_Address, Import;
   begin
      Utils.Print_String (LF & "Causing data abort for null pointer write ..." & LF);
      X := 1;
   end Cause_Data_Abort_Null_Pointer_Write;

begin
   Utils.Print_String (
      LF & Board.Board_Name & " AArch64 Memory Protection - built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time &
      ", boot address ");
   Utils.Print_Number_Hexadecimal (
      Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
      End_Line => True);

   CPU.Memory_Protection.Configure_Global_Regions;

   loop
      Utils.Print_String (LF & "1. Data abort for unmapped address write" & LF);
      Utils.Print_String ("2. Data abort for null pointer write" & LF);
      Utils.Print_String ("3. Breakpoint" & LF);
      C := Utils.Get_Char;
      case C is
         when '1' =>
            Cause_Data_Abort_Unmapped_Write;
         when '2' =>
            Cause_Data_Abort_Null_Pointer_Write;
         when '3' =>
            CPU.Break_Point;
         when others =>
            Utils.Print_String (LF & "Invalid option: '");
            Utils.Put_Char (C);
            Utils.Print_String ("'" & LF);
      end case;
   end loop;
end App_Main;
