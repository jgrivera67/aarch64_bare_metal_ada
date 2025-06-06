--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with CPU;
with CPU.Memory_Protection;
with Gdb_Server;
with Utils.Runtime_Log;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   use ASCII;
   use Utils.Runtime_Log;
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

   procedure Cause_Prefetch_Abort_Executing_From_Stack is
      X : Interfaces.Unsigned_32;
      procedure Bad_Boy
         with Import, Address => X'Address;
   begin
      Utils.Print_String (LF & "Causing prefetch abort executing from the stack ..." & LF);
      Bad_Boy;
   end Cause_Prefetch_Abort_Executing_From_Stack;

   procedure Cause_Prefetch_Abort_Executing_From_Null_Address is
      procedure Bad_Boy
         with Import, Address => System.Null_Address;
   begin
      Utils.Print_String (LF & "Causing prefetch abort executing from null address ..." & LF);
      Bad_Boy;
   end Cause_Prefetch_Abort_Executing_From_Null_Address;

begin
   Gdb_Server.Debug_On := True;
   CPU.Memory_Protection.Debug_On := True;
   --  Utils.Debug_On := True;
   Set_Console_Logging_Level (DEBUG);

   Utils.Print_String (
      LF & Board.Board_Name & " AArch64 Memory Protection - built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time &
      ", boot address ");
   Utils.Print_Number_Hexadecimal (
      Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
      End_Line => True);

   CPU.Memory_Protection.Initialize;

   loop
      Utils.Print_String (LF & "1. Data abort for unmapped address write" & LF);
      Utils.Print_String ("2. Data abort for null pointer write" & LF);
      Utils.Print_String ("3. Breakpoint" & LF);
      Utils.Print_String ("4. Prefetch abort executing from the stack" & LF);
      Utils.Print_String ("5. Prefetch abort executing from null address" & LF);
      C := Utils.Get_Char;
      case C is
         when '1' =>
            Cause_Data_Abort_Unmapped_Write;
         when '2' =>
            Cause_Data_Abort_Null_Pointer_Write;
         when '3' =>
            CPU.Break_Point;
         when '4' =>
            Cause_Prefetch_Abort_Executing_From_Stack;
         when '5' =>
            Cause_Prefetch_Abort_Executing_From_Null_Address;
         when others =>
            Utils.Print_String (LF & "Invalid option: '");
            Utils.Put_Char (C);
            Utils.Print_String ("'" & LF);
      end case;
   end loop;
end App_Main;
