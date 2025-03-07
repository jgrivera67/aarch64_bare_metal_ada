--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces;
with System.Storage_Elements;
with Utils;
with GNAT.Source_Info;

procedure App_Main is
   Code_Address : constant System.Address := Utils.Get_Code_Location_Here;
begin
   for I in 1 .. 8 loop
      Utils.Print_String (
      "Raspberry PI 4 Hello Ada (built on " &
      GNAT.Source_Info.Compilation_Date &
      " at " & GNAT.Source_Info.Compilation_Time &
      ") from address ");
      Utils.Print_Number_Hexadecimal (
         Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Code_Address)),
         End_Line => True);
   end loop;
end App_Main;
