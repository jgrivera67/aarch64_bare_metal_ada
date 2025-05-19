--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary CPU caches utilities
--

with System.Storage_Elements;

package CPU.Caches is

   procedure Enable_Caches
      with Pre => Cpu_In_Privileged_Mode and then
                  Mmu_Is_Enabled,
           Post => Caches_Are_Enabled;

   procedure Disable_Caches
      with Pre => Cpu_In_Privileged_Mode and then
                  not Mmu_Is_Enabled,
           Post => not Caches_Are_Enabled;

   procedure Invalidate_Data_Cache;

   procedure Invalidate_Instruction_Cache;

   procedure Invalidate_Data_Cache_Line (Cache_Line_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Cache_Line_Address) mod
                     Cache_Line_Size_In_Bytes = 0;

   use type System.Address;

   procedure Invalidate_Data_Cache_Range (Start_Address : System.Address;
                                          End_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Start_Address) mod
                     Cache_Line_Size_In_Bytes = 0 and then
                   System.Storage_Elements.To_Integer (End_Address) mod
                     Cache_Line_Size_In_Bytes = 0 and then
                   Start_Address < End_Address;

   procedure Flush_Data_Cache_Line (Cache_Line_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Cache_Line_Address) mod
                     Cache_Line_Size_In_Bytes = 0;

   procedure Flush_Invalidate_Data_Cache_Line (Cache_Line_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Cache_Line_Address) mod
                     Cache_Line_Size_In_Bytes = 0;

   procedure Flush_Invalidate_Data_Cache_Range (Start_Address : System.Address;
                                                End_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Start_Address) mod
                     Cache_Line_Size_In_Bytes = 0 and then
                   System.Storage_Elements.To_Integer (End_Address) mod
                     Cache_Line_Size_In_Bytes = 0 and then
                   Start_Address < End_Address;

end CPU.Caches;