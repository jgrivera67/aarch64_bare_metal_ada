--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with CPU;
with Utils;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   use ASCII;
   Code_Address : constant System.Address := CPU.Get_Reset_Handler_Address;
   C : Character;

   procedure Cause_Data_Abort is
      Invalid_Address : constant System.Address := System.Storage_Elements.To_Address (16#ff00_0000_0000#);
      X : Interfaces.Unsigned_8 with Address => Invalid_Address, Import;
   begin
      Utils.Print_String (LF & "Causing data abort..." & LF);
      X := 1;
   end Cause_Data_Abort;

   procedure Execute_Break_Instruction is
   begin
      CPU.Break_Point;
   end Execute_Break_Instruction;

   procedure Cause_Assert_Failure is
   begin
      pragma Assert (False, "This is an assert failure for testing purposes");
   end Cause_Assert_Failure;

   procedure Raise_Ada_Exception is
   begin
      raise Program_Error with "This is an Ada exception for testing purposes";
   end Raise_Ada_Exception;

begin
   Utils.Print_String (
      LF & Board.Board_Name & " AArch64 Exceptions - built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time &
      ", boot address ");
   Utils.Print_Number_Hexadecimal (
      Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
      End_Line => True);

   loop
      Utils.Print_String (LF & "1. Data abort" & LF);
      Utils.Print_String ("2. Break instruction" & LF);
      Utils.Print_String ("3. Assert failure" & LF);
      Utils.Print_String ("4. Raise Ada exception" & LF);
      C := Utils.Get_Char;
      case C is
         when '1' =>
            Cause_Data_Abort;
         when '2' =>
            Execute_Break_Instruction;
         when '3' =>
            Cause_Assert_Failure;
         when '4' =>
            Raise_Ada_Exception;
         when others =>
            Utils.Print_String (LF & "Invalid option: '");
            Utils.Put_Char (C);
            Utils.Print_String ("'" & LF);
      end case;
   end loop;
end App_Main;
