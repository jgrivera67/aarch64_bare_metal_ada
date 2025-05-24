--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with CPU.Self_Hosted_Debug;
with System;

package Gdb_Server with SPARK_Mode => On is
   procedure Run_Gdb_Server (
      Debug_Event : CPU.Self_Hosted_Debug.Debug_Event_Type;
      Current_PC : in out System.Address)
      with Pre => CPU.Cpu_In_Privileged_Mode and then
                  CPU.Cpu_Interrupting_Disabled;

private

   type Gdb_Server_Type is limited record
      Gdb_Attached : Boolean := False;
   end record;

   Gdb_Server_Obj : Gdb_Server_Type;
end Gdb_Server;
