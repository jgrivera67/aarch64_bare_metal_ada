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

package body CPU.Multicore is

   MPIDR_Core_Id_Mask : constant := 2#1111_1111#;

   function Get_Cpu_Id return Cpu_Core_Id_Type is
      use type Interfaces.Unsigned_64;
      Reg_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, mpidr_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", Reg_Value), --  %0
         Volatile => True);

      Reg_Value := @ and MPIDR_Core_Id_Mask;
      return Cpu_Core_Id_Type (Reg_Value);
   end Get_Cpu_Id;

   procedure Wait_For_Multicore_Event is
   begin
      System.Machine_Code.Asm ("wfe", Volatile => True);
   end Wait_For_Multicore_Event;

   procedure Send_Multicore_Event is
   begin
      System.Machine_Code.Asm ("sev", Volatile => True);
   end Send_Multicore_Event;

   procedure Secondary_Cpu_Main is
   begin
      Park_Cpu; -- TODO: Replace this with real logic
   end Secondary_Cpu_Main;

end CPU.Multicore;