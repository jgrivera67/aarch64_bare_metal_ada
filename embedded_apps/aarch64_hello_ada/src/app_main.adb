--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Board;
with Utils;
with Interfaces;
with System.Storage_Elements;
with GNAT.Source_Info;

procedure App_Main is
   Code_Address : constant System.Address := Utils.Get_Code_Location_Here;
begin
   Utils.Print_String (
      ASCII.LF & Board.Board_Name & " Hello Ada (built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time &
      ") from address ");
   Utils.Print_Number_Hexadecimal (
      Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
      End_Line => True);
end App_Main;
