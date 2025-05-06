--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary CPU caches utilities
--

with System.Machine_Code;

package body CPU.Caches is

   procedure Enable_Caches is
      SCTLR_Value : SCTLR_EL1_Type;
   begin
      Memory_Barrier;
      Invalidate_Data_Cache;
      Invalidate_Instruction_Cache;
      SCTLR_Value := Get_SCTLR_EL1;
      SCTLR_Value.C := Cacheable;
      SCTLR_Value.I := Instruction_Access_Cacheable;
      Set_SCTLR_EL1 (SCTLR_Value);
      Strong_Memory_Barrier;
   end Enable_Caches;

   procedure Disable_Caches is
      SCTLR_Value : SCTLR_EL1_Type;
   begin
      Strong_Memory_Barrier;
      SCTLR_Value := Get_SCTLR_EL1;
      SCTLR_Value.C := Non_Cacheable;
      SCTLR_Value.I := Instruction_Access_Non_Cacheable;
      Set_SCTLR_EL1 (SCTLR_Value);
      Strong_Memory_Barrier;
   end Disable_Caches;

   procedure Invalidate_Data_Cache is
   begin
      Strong_Memory_Barrier;
      System.Machine_Code.Asm (
         "dc zva, xzr",
         Clobber => "memory",
         Volatile => True);
      Strong_Memory_Barrier;
   end Invalidate_Data_Cache;

   procedure Invalidate_Instruction_Cache is
   begin
      Strong_Memory_Barrier;
      System.Machine_Code.Asm (
         "ic iallu",
         Clobber => "memory",
         Volatile => True);
      Strong_Memory_Barrier;
   end Invalidate_Instruction_Cache;

   procedure Invalidate_Data_Cache_Line (Cache_Line_Address : System.Address) is
   begin
      Strong_Memory_Barrier;
      System.Machine_Code.Asm (
         "dc ivac, %0",
         Inputs => System.Address'Asm_Input ("r", Cache_Line_Address), --  %0
         Clobber => "memory",
         Volatile => True);
      Strong_Memory_Barrier;
   end Invalidate_Data_Cache_Line;

   procedure Flush_Data_Cache_Line (Cache_Line_Address : System.Address) is
   begin
      Strong_Memory_Barrier;
      System.Machine_Code.Asm (
         "dc cvac, %0",
         Inputs => System.Address'Asm_Input ("r", Cache_Line_Address), --  %0
         Volatile => True);
      Strong_Memory_Barrier;
   end Flush_Data_Cache_Line;

   procedure Flush_Invalidate_Data_Cache_Line (Cache_Line_Address : System.Address) is
   begin
      Strong_Memory_Barrier;
      System.Machine_Code.Asm (
         "dc civac, %0",
         Inputs => System.Address'Asm_Input ("r", Cache_Line_Address), --  %0
         Clobber => "memory",
         Volatile => True);
      Strong_Memory_Barrier;
   end Flush_Invalidate_Data_Cache_Line;

end CPU.Caches;