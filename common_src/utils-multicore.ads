--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Multicore Utilities
--

package Utils.Multicore
   with SPARK_Mode => On
is
   Num_Cpu_Cores : constant := 4;

   type Cpu_Core_Id_Type is mod Num_Cpu_Cores;

   function Get_Cpu_Id return Cpu_Core_Id_Type
      with Inline_Always,
           Suppress => All_Checks;

end Utils.Multicore;
