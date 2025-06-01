--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Memory map from the linker script
--
with System;
private with Interfaces;

package Linker_Memory_Map is
   Global_Text_Region_Start_Address : constant System.Address;
   Global_Text_Region_End_Address : constant System.Address;

   Global_Rodata_Region_Start_Address : constant System.Address;
   Global_Rodata_Region_End_Address : constant System.Address;

   Global_Data_Region_Start_Address : constant System.Address;
   Global_Data_Region_End_Address : constant System.Address;

   Stacks_End_Address : constant System.Address;

private

   --
   --  Linker-script symbols defined in
   --  porting_layer/platforms/*/memory_layout.ld
   --

   Global_Text_Region_Start_Linker_Symbol : constant Interfaces.Unsigned_8
      with Import,
           Convention => Asm,
           External_Name => "__global_text_region_start";

   Global_Text_Region_Start_Address : constant System.Address :=
      Global_Text_Region_Start_Linker_Symbol'Address;

   Global_Text_Region_End_Linker_Symbol : constant Interfaces.Unsigned_8
      with Import,
           Convention => Asm,
           External_Name => "__global_text_region_end";

   Global_Text_Region_End_Address : constant System.Address :=
      Global_Text_Region_End_Linker_Symbol'Address;

   Global_Rodata_Region_Start_Linker_Symbol : constant Interfaces.Unsigned_8
      with Import,
           Convention => Asm,
           External_Name => "__global_rodata_region_start";

   Global_Rodata_Region_Start_Address : constant System.Address :=
      Global_Rodata_Region_Start_Linker_Symbol'Address;

   Global_Rodata_Region_End_Linker_Symbol : constant Interfaces.Unsigned_8
      with Import,
           Convention => Asm,
           External_Name => "__global_rodata_region_end";

   Global_Rodata_Region_End_Address : constant System.Address :=
      Global_Rodata_Region_End_Linker_Symbol'Address;

   Global_Data_Region_Start_Linker_Symbol : Interfaces.Unsigned_8
      with Import,
           Convention => Asm,
           External_Name => "__global_data_region_start";

   Global_Data_Region_Start_Address : constant System.Address :=
      Global_Data_Region_Start_Linker_Symbol'Address;

   Global_Data_Region_End_Linker_Symbol : Interfaces.Unsigned_8
      with Import,
           Convention => Asm,
           External_Name => "__global_data_region_end";

   Global_Data_Region_End_Address : constant System.Address :=
      Global_Data_Region_End_Linker_Symbol'Address;

   Stacks_End_Linker_Symbol : Interfaces.Unsigned_8
      with Import,
           Convention => Asm,
           External_Name => "__stacks_end";

   Stacks_End_Address : constant System.Address :=
      Stacks_End_Linker_Symbol'Address;

end Linker_Memory_Map;
