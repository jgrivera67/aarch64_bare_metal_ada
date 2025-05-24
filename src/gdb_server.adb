--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--
with Interfaces;
with Utils;
with System.Storage_Elements;

package body Gdb_Server is
   use System.Storage_Elements;

   procedure Run_Gdb_Server (
      Debug_Event : CPU.Self_Hosted_Debug.Debug_Event_Type;
      Current_PC : in out System.Address)
   is
      C : Character;
   begin
      if not Gdb_Server_Obj.Gdb_Attached then
         Utils.Lock_Console (Print_Cpu => True);
         Utils.Print_String (ASCII.LF & "Entering self-hosted debugger on ");
         Utils.Print_String (Debug_Event'Image);
         Utils.Print_String (" at PC ");
         Utils.Print_Number_Hexadecimal (Interfaces.Unsigned_64 (To_Integer (Current_PC)),
                                       End_Line => True);
         Utils.Unlock_Console;
         CPU.Self_Hosted_Debug.Enable_Self_Hosted_Debugging;
      end if;

      loop
         null;
      end loop;

      if not Gdb_Server_Obj.Gdb_Attached then
         CPU.Self_Hosted_Debug.Disable_Self_Hosted_Debugging;
         Utils.Lock_Console (Print_Cpu => True);
         Utils.Print_String (ASCII.LF & "Exiting self-hosted debugger at PC ");
         Utils.Print_Number_Hexadecimal (Interfaces.Unsigned_64 (To_Integer (Current_PC)),
                                         End_Line => True);
         Utils.Unlock_Console;
      end if;

   end Run_Gdb_Server;
end Gdb_Server;