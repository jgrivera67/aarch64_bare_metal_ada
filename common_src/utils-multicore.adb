--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Multicore Utilities
--

with System.Machine_Code;
with Interfaces;

package body Utils.Multicore is

   MPIDR_Core_Id_Mask : constant := 2#1111_1111#;

   function Get_Cpu_Id return Cpu_Core_Id_Type is
      Reg_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, mpidr_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", Reg_Value), --  %0
         Volatile => True);

      Reg_Value := @ and MPIDR_Core_Id_Mask;
      return Cpu_Core_Id_Type (Reg_Value);
   end Get_Cpu_Id;

end Utils.Multicore;