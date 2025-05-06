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
      with Pre => Cpu_In_Privileged_Mode;

   procedure Disable_Caches
      with Pre => Cpu_In_Privileged_Mode;

   procedure Invalidate_Data_Cache;

   procedure Invalidate_Instruction_Cache;

   procedure Invalidate_Data_Cache_Line (Cache_Line_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Cache_Line_Address) mod
                     Cache_Line_Size_In_Bytes = 0;

   procedure Flush_Data_Cache_Line (Cache_Line_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Cache_Line_Address) mod
                     Cache_Line_Size_In_Bytes = 0;

   procedure Flush_Invalidate_Data_Cache_Line (Cache_Line_Address : System.Address)
      with Pre => System.Storage_Elements.To_Integer (Cache_Line_Address) mod
                     Cache_Line_Size_In_Bytes = 0;
end CPU.Caches;