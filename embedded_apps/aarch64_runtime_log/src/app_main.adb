--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with CPU;
with Utils.Runtime_Log;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   use Utils.Runtime_Log;
   Code_Address : constant System.Address := CPU.Get_Reset_Handler_Address;
begin
   Utils.Print_String (
      ASCII.LF & Board.Board_Name & " Runtime log - built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time &
      ", boot address ");
   Utils.Print_Number_Hexadecimal (
      Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
      End_Line => True);

   Set_Console_Logging_Level (DEBUG);
   Log_Debug_Msg ("This is a debug message");
   Log_Debug_Msg_Begin ("This is a debug message with arg: ");
   Log_Debug_Value_Hexadecimal (Interfaces.Unsigned_64 (
      System.Storage_Elements.To_Integer (Utils.Get_PC_Here)));
   Log_Debug_Msg_End;

   Log_Info_Msg ("This is an info message");
   Log_Info_Msg_Begin ("This is an info message with arg: ");
   Log_Info_Value_Hexadecimal (Interfaces.Unsigned_64 (
      System.Storage_Elements.To_Integer (Utils.Get_PC_Here)));
   Log_Info_Msg_End;

   Log_Error_Msg ("This is an error message");
   Log_Error_Msg_Begin ("This is an error message with arg: ");
   Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (
      System.Storage_Elements.To_Integer (Utils.Get_PC_Here)));
   Log_Error_Msg_End;

   CPU.Park_Cpu;
end App_Main;
