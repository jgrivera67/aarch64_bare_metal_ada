--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary CPU Utilities
--

with System.Machine_Code;

package body CPU is

   function Get_Call_Address return System.Address is
      use type Interfaces.Unsigned_64;
      Reg_Value : Interfaces.Unsigned_64;
      Call_Instruction_Size_In_Bytes : constant := 4;
   begin
      System.Machine_Code.Asm (
         "mov %0, lr",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", Reg_Value),
         Volatile => True);

      return System.Storage_Elements.To_Address (
               System.Storage_Elements.Integer_Address (
                  Reg_Value - Call_Instruction_Size_In_Bytes));
   end Get_Call_Address;

   procedure Memory_Barrier is
   begin
      System.Machine_Code.Asm ("dmb sy",
         Clobber => "memory",
         Volatile => True);
   end Memory_Barrier;

   procedure Strong_Memory_Barrier is
   begin
      System.Machine_Code.Asm (
         "dsb sy"  & ASCII.LF &
         "isb",
         Clobber => "memory",
         Volatile => True);
   end Strong_Memory_Barrier;

   procedure Break_Point is
   begin
      System.Machine_Code.Asm ("brk #0", Volatile => True);
   end Break_Point;

   function Get_CurrentEL return Exception_Level_Type is
      PSTATE_Value : PSTATE_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, currentel",
         Outputs => Cpu_Register_Type'Asm_Output ("=r", PSTATE_Value.Value),
         Volatile => True);

      return PSTATE_Value.CurrentEL;
   end Get_CurrentEL;

   function Get_DAIF return DAIF_Type is
      PSTATE_Value : PSTATE_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, daif",
         Outputs => Cpu_Register_Type'Asm_Output ("=r", PSTATE_Value.Value),
         Volatile => True);

      return PSTATE_Value.DAIF;
   end Get_DAIF;

end CPU;