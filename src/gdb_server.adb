--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--
with Utils;
with CPU.Interrupt_Handling;
with System.Storage_Elements;

package body Gdb_Server is

   procedure Run_Gdb_Server is
      use System.Storage_Elements;
      Saved_PC : constant System.Address := CPU.Interrupt_Handling.Get_Saved_PC;
      C : Character;
   begin
      Utils.Print_String (ASCII.LF & "Running GDB server ..." & ASCII.LF);
      Utils.Print_String ("GDB server not implemented yet" & ASCII.LF);
      loop
         C := Utils.Get_Char;
         --???
         if C = ASCII.CR then
            Utils.Put_Char (ASCII.LF);
         else
            Utils.Put_Char (C);
            exit when C = 'q';
         end if;
         --???
      end loop;

      CPU.Interrupt_Handling.Set_Saved_PC (
         To_Address (To_Integer (Saved_PC) + CPU.Instruction_Size_In_Bytes));
   end Run_Gdb_Server;
end Gdb_Server;