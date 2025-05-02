--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Multicore Utilities
--

package CPU.Multicore
   with SPARK_Mode => On
is
   Num_Cpu_Cores : constant := 4;

   type Cpu_Core_Id_Type is mod Num_Cpu_Cores;

   function Get_Cpu_Id return Cpu_Core_Id_Type
      with Inline_Always,
           Suppress => All_Checks;

   procedure Wait_For_Multicore_Event with
      Inline_Always;

   procedure Send_Multicore_Event with
      Inline_Always;

   procedure Secondary_Cpu_Main
      with Export,
           Convention => C,
           External_Name => "secondary_cpu_main",
           No_Return;
end CPU.Multicore;
