--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Memory protection services for ARMv8-A MMU
--

with Board;
with CPU.Interrupt_Handling;
with Utils;
with System.Machine_Code;

package body CPU.Memory_Protection is

   procedure Configure_Global_Regions is
      Cpu_Id : constant Cpu_Core_Id_Type := Get_Cpu_Id;
      Translation_Table_Tree : Translation_Table_Tree_Type renames
         Translation_Table_Trees (Cpu_Id);
   begin
      Utils.Print_String ("Configuring MMU translation tables ..." & ASCII.LF);
      Initialize;

      --
      --  Configure global text region:
      --
      Configure_Memory_Region (Linker_Memory_Map.Global_Text_Region_Start_Address,
                               Linker_Memory_Map.Global_Text_Region_End_Address,
                               Unprivileged_Permissions => Read_Execute,
                               Privileged_Permissions => Read_Execute,
                               Region_Attributes =>
                                 --  Only reads need to be cached
                                 Normal_Memory_Write_Through_Cacheable);

      --
      --  Set NULL pointer de-reference guard region:
      --
      if Linker_Memory_Map.Global_Text_Region_Start_Address /= System.Null_Address then
         declare
            Page_Size_In_Dwords : constant :=
               Page_Size_In_Bytes / (Interfaces.Unsigned_64'Size / System.Storage_Unit);
            Null_Page : array (1 .. Page_Size_In_Dwords) of Interfaces.Unsigned_64
               with Import, Address => System.Null_Address;
         begin
            Null_Page := [others => Interfaces.Unsigned_64'Last];
            Configure_Memory_Region (Start_Address => System.Null_Address,
                                    Size_In_Bytes => Page_Size_In_Bytes,
                                    Unprivileged_Permissions => None,
                                    --  NOTE: PERM_NONE not supported for privileged mode in ARMv8-A MMU
                                    Privileged_Permissions => Read_Only,
                                    Region_Attributes => Normal_Memory_Non_Cacheable);
         end;
      end if;

      --
      --  Configure global rodata region:
      --
      Configure_Memory_Region (Linker_Memory_Map.Global_Rodata_Region_Start_Address,
                               Linker_Memory_Map.Global_Rodata_Region_End_Address,
                               Unprivileged_Permissions => Read_Only,
                               Privileged_Permissions => Read_Only,
                               Region_Attributes =>
                                 --  Only reads need to be cached
                                 Normal_Memory_Write_Through_Cacheable);

      --
      --  Configure global data region:
      --
      Configure_Memory_Region (Linker_Memory_Map.Global_Data_Region_Start_Address,
                               Linker_Memory_Map.Global_Data_Region_End_Address,
                               Unprivileged_Permissions => Read_Write,
                               Privileged_Permissions => Read_Write,
                               Region_Attributes => Normal_Memory_Write_Back_Cacheable);

      --
      --  Configure region to detect ISR stack overflows:
      --
      Configure_Memory_Region (
         CPU.Interrupt_Handling.ISR_Stacks (Cpu_Id).Stack_Overflow_Guard'Address,
         CPU.Interrupt_Handling.ISR_Stacks (Cpu_Id).Stack_Overflow_Guard'Size / System.Storage_Unit,
         Unprivileged_Permissions => None,
         --  NOTE: PERM_NONE not supported for privileged mode
         Privileged_Permissions => Read_Only,
         Region_Attributes => Normal_Memory_Non_Cacheable);

      --
      --  Configure ISR stack region:
      --
      Configure_Memory_Region (CPU.Interrupt_Handling.ISR_Stacks (Cpu_Id).Stack_Entries'Address,
                               CPU.Interrupt_Handling.ISR_Stack_Size_In_Bytes,
                               Unprivileged_Permissions => Read_Write,
                               Privileged_Permissions => Read_Write,
                               Region_Attributes => Normal_Memory_Write_Back_Cacheable);

      --
      --  Configure global MMIO region:
      --
      Configure_Memory_Region (Board.Global_Mmio_Region_Start_Address,
                               Board.Global_Mmio_Region_End_Address,
                               Unprivileged_Permissions => None,
                               Privileged_Permissions => Read_Write,
                               Region_Attributes => Device_Memory_Mapped_Io);

      --
      --  Configure MMU translation tables region:
      --
      Configure_Memory_Region (Translation_Table_Tree'Address,
                               Translation_Table_Tree'Size / System.Storage_Unit,
                               Unprivileged_Permissions => None,
                               Privileged_Permissions => Read_Write,
                               Region_Attributes => Normal_Memory_Non_Cacheable);

      Utils.Print_String ("Before enabling MMU" & ASCII.LF); --???
      Enable_MMU;
      Utils.Print_String ("MMU enabled" & ASCII.LF);
   end Configure_Global_Regions;

   procedure Initialize is
      procedure Load_Memory_Attributes_Lookup_Table is
         MAIR_Value : MAIR_Type;
      begin
         --
         --  Load all memory attributes supported into MAIR_EL1 register:
         --
         MAIR_Value.Attr_Array := Memory_Attributes_Lookup_Table;
         Set_MAIR (MAIR_Value);
      end Load_Memory_Attributes_Lookup_Table;

      procedure Initialize_Translation_Table_Tree (
         Translation_Table_Tree : out Translation_Table_Tree_Type) is
      begin
         for L1_Index in Level1_Translation_Table_Entry_Index_Type loop
            declare
               L1_Entry : Translation_Table_Entry_Type renames
                 Translation_Table_Tree.Level1_Translation_Table (L1_Index);
               L2_Table : Translation_Table_Type renames
                  Translation_Table_Tree.Level2_Translation_Tables (L1_Index);
            begin
               L1_Entry := (others => <>);
               L1_Entry.Physical_Page_Address_Prefix :=
                  Address_To_Page_Address_Prefix (L2_Table'Address);
               L1_Entry.Entry_Kind := Translation_Table_Entry_Is_Table_Or_Page;
               L1_Entry.Valid_Entry := True;
               for L2_Index in Translation_Table_Entry_Index_Type loop
                  declare
                     L2_Entry : Translation_Table_Entry_Type renames
                        L2_Table (L2_Index);
                  begin
                     L2_Entry := (others => <>);
                     L2_Entry.Physical_Page_Address_Prefix :=
                        Address_To_Page_Address_Prefix (
                           Translation_Table_Tree.Level3_Translation_Tables (L1_Index, L2_Index)'Address);
                     L2_Entry.Entry_Kind := Translation_Table_Entry_Is_Table_Or_Page;
                     L2_Entry.Valid_Entry := True;
                  end;
               end loop;
            end;
         end loop;
      end Initialize_Translation_Table_Tree;

      TCR_Value : TCR_Type;
      TTBR0_Value : TTBRn_Type;
      Cpu_Id : constant Cpu_Core_Id_Type := Get_Cpu_Id;
      Translation_Table_Tree : Translation_Table_Tree_Type renames Translation_Table_Trees (Cpu_Id);
      Level1_Translation_Table_Address : constant System.Address :=
         Translation_Table_Tree.Level1_Translation_Table'Address;
   begin
      pragma Assert (Level3_Translation_Table_Entry_Range_Size = Page_Size_In_Bytes);
      pragma Assert (
         Virtual_Address_Space_Size_In_Bytes <=
            Max_Num_Translation_Table_Entries * Level1_Translation_Table_Entry_Range_Size);
      pragma Assert (
         Virtual_Address_Space_Size_In_Bytes mod Level1_Translation_Table_Entry_Range_Size = 0);

      Load_Memory_Attributes_Lookup_Table;

      --
      --  Configure translation regime:
      --  - Normal memory, Inner Non-cacheable and Outer Non-cacheable for
      --    translation table memory
      --  - Non-Sharable translation table memory
      --  - 4KB granule size
      --  - Only TTBR0 will be used
      --
      TCR_Value := (@ with delta T0SZ => Virtual_Address_Space_Size_TnSZ_Value,
                                 SH0 => Non_Shareable,
                                 TG0 => TTBR0_Granule,
                                 EPD1 => TTBR1_Translation_Table_Walk_Disabled);
      Set_TCR (TCR_Value);

      Initialize_Translation_Table_Tree (Translation_Table_Tree);

      --
      --  Set TTBR0 to point to the level1 translation table:
      --
      pragma Assert (Address_Is_Page_Aligned (Level1_Translation_Table_Address));
      TTBR0_Value.Value := Interfaces.Unsigned_64 (To_Integer (Level1_Translation_Table_Address));
      Set_TTBR0 (TTBR0_Value);
   end Initialize;

   procedure Configure_Memory_Region (
      Start_Address : System.Address;
      Size_In_Bytes : Integer_Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      Translation_Table_Tree : Translation_Table_Tree_Type renames
         Translation_Table_Trees (Get_Cpu_Id);
   begin
      Populate_Page_Translation_Tables (Translation_Table_Tree,
                                        Start_Address,
                                        Size_In_Bytes,
                                        Unprivileged_Permissions,
                                        Privileged_Permissions,
                                        Region_Attributes);
   end Configure_Memory_Region;

   procedure Configure_Memory_Region (
      Start_Address : System.Address;
      End_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      Translation_Table_Tree : Translation_Table_Tree_Type renames
         Translation_Table_Trees (Get_Cpu_Id);
   begin
      Populate_Page_Translation_Tables (Translation_Table_Tree,
                                        Start_Address,
                                        End_Address,
                                        Unprivileged_Permissions,
                                        Privileged_Permissions,
                                        Region_Attributes);
   end Configure_Memory_Region;

   procedure Populate_Page_Translation_Tables (
      Translation_Table_Tree : in out Translation_Table_Tree_Type;
      Start_Address : System.Address;
      Size_In_Bytes : Integer_Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      End_Address : constant System.Address := To_Address (
         To_Integer (Start_Address) + Size_In_Bytes);
   begin
      Populate_Page_Translation_Tables (Translation_Table_Tree,
                                        Start_Address,
                                        End_Address,
                                        Unprivileged_Permissions,
                                        Privileged_Permissions,
                                        Region_Attributes);
   end Populate_Page_Translation_Tables;

   procedure Populate_Page_Translation_Tables (
      Translation_Table_Tree : in out Translation_Table_Tree_Type;
      Start_Address : System.Address;
      End_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Page_Attributes : Region_Attributes_Type) is
      Physical_Address : System.Address := Start_Address;
      L1_Index : Translation_Table_Entry_Index_Type :=
         Translation_Table_Entry_Index_Type (To_Integer (Start_Address) /
                                             Level1_Translation_Table_Entry_Range_Size);
      L2_Index : Translation_Table_Entry_Index_Type :=
         Translation_Table_Entry_Index_Type (
            (To_Integer (Start_Address) mod Level1_Translation_Table_Entry_Range_Size) /
            Level2_Translation_Table_Entry_Range_Size);
      L3_Index : Translation_Table_Entry_Index_Type :=
         Translation_Table_Entry_Index_Type (
            (To_Integer (Start_Address) mod Level2_Translation_Table_Entry_Range_Size) /
            Level3_Translation_Table_Entry_Range_Size);
   begin
      loop
         pragma Loop_Invariant (Physical_Address >= Start_Address and then
                                Physical_Address < End_Address);
         declare
            Level3_Translation_Table : Translation_Table_Type renames
               Translation_Table_Tree.Level3_Translation_Tables (L1_Index, L2_Index);
            L3_Entry : Translation_Table_Entry_Type renames
               Level3_Translation_Table (L3_Index);
         begin
            L3_Entry := (others => <>);
            Populate_Level3_Translation_Table_Entry (
               L3_Entry,
               Physical_Address,
               Unprivileged_Permissions,
               Privileged_Permissions,
               Page_Attributes);

            Physical_Address :=
               To_Address (To_Integer (@) + Level3_Translation_Table_Entry_Range_Size);
            exit when Physical_Address = End_Address;

            L3_Index := @ + 1;
            if L3_Index = 0 then
               L2_Index := @ + 1;
               if L2_Index = 0 then
                  L1_Index := @ + 1;
               end if;
            end if;
         end;
      end loop;
   end Populate_Page_Translation_Tables;

   procedure Populate_Level3_Translation_Table_Entry (
      Translation_Table_Entry : out Translation_Table_Entry_Type;
      Start_Physical_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Page_Attributes : Region_Attributes_Type)
   is
   begin
      Translation_Table_Entry.SH := Inner_Shareable;
      Translation_Table_Entry.PXN := Non_Executable;
      Translation_Table_Entry.UXN := Non_Executable;
      case Privileged_Permissions is
         when Read_Write =>
            if Unprivileged_Permissions = Read_Write then
               Translation_Table_Entry.AP := EL1_and_EL0_Read_Write; --  same as EL2_Read_Write_EL1_EL0_No_Access
            else
               pragma Assert (Unprivileged_Permissions = None);
               Translation_Table_Entry.AP := EL1_Read_Write_EL0_No_Access; --  same as EL2_Read_Write_EL1_EL0_No_Access
            end if;
         when Read_Only =>
            if Unprivileged_Permissions = Read_Only then
               Translation_Table_Entry.AP := EL1_and_EL0_Read_Only; --  same as EL2_and_EL1_EL0_Read_Only
            else
               pragma Assert (Unprivileged_Permissions = None);
               Translation_Table_Entry.AP := EL1_Read_Only_EL0_No_Access; --  same as EL2_Read_Only_EL1_EL0_No_Access
            end if;
         when Read_Execute =>
            Translation_Table_Entry.PXN := Executable;
            if Unprivileged_Permissions = Read_Execute then
               Translation_Table_Entry.UXN := Executable;
               Translation_Table_Entry.AP := EL1_and_EL0_Read_Only; --  same as EL2_and_EL1_EL0_Read_Only
            else
               pragma Assert (Unprivileged_Permissions = None);
               Translation_Table_Entry.AP := EL1_Read_Only_EL0_No_Access; --  same as EL2_Read_Only_EL1_EL0_No_Access
            end if;
         when Read_Write_Execute =>
            pragma Assert (Unprivileged_Permissions = Read_Write_Execute);
            Translation_Table_Entry.PXN := Executable;
            Translation_Table_Entry.AP := EL1_and_EL0_Read_Write; --  same as EL2_Read_Write_EL1_EL0_No_Access
         when None =>
            null;
      end case;

      pragma Assert (Address_Is_Page_Aligned (Start_Physical_Address));
      Translation_Table_Entry.Physical_Page_Address_Prefix :=
         Address_To_Page_Address_Prefix (Start_Physical_Address);
      Translation_Table_Entry.Attr_Index := Translation_Table_MAIR_EL1_Index_Type (Page_Attributes'Enum_Rep);
      Translation_Table_Entry.AF := True;
      Translation_Table_Entry.NS := True;
      Translation_Table_Entry.Entry_Kind := Translation_Table_Entry_Is_Table_Or_Page;
      Translation_Table_Entry.Valid_Entry := True;
   end Populate_Level3_Translation_Table_Entry;

   procedure Handle_Prefetch_Abort_Exception is
   begin
      null; --  TODO: Implement this

      --  HiRTOS_Low_Level_Debug_Interface.Print_String (
      --     "*** EL1 Prefetch abort: " & Fault_Name_Pointer_Array (IFSR_Value.Status).all & "  (faulting PC: ");
      --  HiRTOS_Low_Level_Debug_Interface.Print_Number_Hexadecimal (Interfaces.Unsigned_32 (IFAR_Value));
      --  HiRTOS_Low_Level_Debug_Interface.Print_String (")" & ASCII.LF);

      raise Program_Error;
   end Handle_Prefetch_Abort_Exception;

   procedure Handle_Data_Abort_Exception is
      --  Faulting_PC : constant Integer_Address :=
      --     To_Integer (Interrupt_Handling.Get_Interrupted_PC) - 8;
   begin
      null; --  TODO: Implement this

      --  HiRTOS_Low_Level_Debug_Interface.Print_String (
      --     "*** EL1 Data abort: " & Fault_Name_Pointer_Array (DFSR_Value.Status).all & "  (faulting PC: ");
      --  HiRTOS_Low_Level_Debug_Interface.Print_Number_Hexadecimal (Interfaces.Unsigned_32 (Faulting_PC));
      --  HiRTOS_Low_Level_Debug_Interface.Print_String (", fault data address: ");
      --  HiRTOS_Low_Level_Debug_Interface.Print_Number_Hexadecimal (Interfaces.Unsigned_32 (DFAR_Value));
      --  HiRTOS_Low_Level_Debug_Interface.Print_String (")" & ASCII.LF);

      raise Program_Error;
   end Handle_Data_Abort_Exception;

   function Get_MAIR return MAIR_Type is
      MAIR_Value : MAIR_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, mair_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", MAIR_Value.Value), --  %0
         Volatile => True);

      return MAIR_Value;
   end Get_MAIR;

   procedure Set_MAIR (MAIR_Value : MAIR_Type) is
   begin
      System.Machine_Code.Asm (
         "msr mair_el1, %0",
         Inputs => Interfaces.Unsigned_64'Asm_Input ("r", MAIR_Value.Value), --  %0
         Volatile => True);
   end Set_MAIR;

   function Get_TCR return TCR_Type is
      TCR_Value : TCR_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, tcr_el1",
         Outputs => TCR_Type'Asm_Output ("=r", TCR_Value), --  %0
         Volatile => True);

      return TCR_Value;
   end Get_TCR;

   procedure Set_TCR (TCR_Value : TCR_Type) is
   begin
      System.Machine_Code.Asm (
         "msr tcr_el1, %0",
         Inputs => TCR_Type'Asm_Input ("r", TCR_Value), --  %0
         Volatile => True);
   end Set_TCR;

   function Get_TTBR0 return TTBRn_Type is
      TTBR0_Value : TTBRn_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, ttbr0_el1",
         Outputs => TTBRn_Type'Asm_Output ("=r", TTBR0_Value), --  %0
         Volatile => True);

      return TTBR0_Value;
   end Get_TTBR0;

   procedure Set_TTBR0 (TTBR0_Value : TTBRn_Type) is
   begin
      System.Machine_Code.Asm (
         "msr ttbr0_el1, %0",
         Inputs => TTBRn_Type'Asm_Input ("r", TTBR0_Value), --  %0
         Volatile => True);
   end Set_TTBR0;

   function Get_TTBR1 return TTBRn_Type is
      TTBR1_Value : TTBRn_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, ttbr1_el1",
         Outputs => TTBRn_Type'Asm_Output ("=r", TTBR1_Value), --  %0
         Volatile => True);

      return TTBR1_Value;
   end Get_TTBR1;

   procedure Set_TTBR1 (TTBR1_Value : TTBRn_Type) is
   begin
      System.Machine_Code.Asm (
         "msr ttbr1_el1, %0",
         Inputs => TTBRn_Type'Asm_Input ("r", TTBR1_Value), --  %0
         Volatile => True);
   end Set_TTBR1;

   procedure Invalidate_TLB is
   begin
      System.Machine_Code.Asm (
         "tlbi vmalle1",
         Clobber => "memory",
         Volatile => True);
   end Invalidate_TLB;

   procedure Enable_MMU is
      SCTLR_Value : SCTLR_EL1_Type;
      Old_Cpu_Interrupting_State : constant Cpu_Register_Type :=
         Interrupt_Handling.Disable_Cpu_Interrupting;
   begin
      Strong_Memory_Barrier;
      Invalidate_TLB;
      Strong_Memory_Barrier;
      SCTLR_Value := Get_SCTLR_EL1;
      SCTLR_Value.M := MMU_Enabled;
      SCTLR_Value.A :=  Alignment_Check_Enabled;
      --SCTLR_Value.SA0 := SP_EL0_Alignment_Check_Enabled;
      --  NOTE: We don't set SA, because we never initialize SP_EL1
      --  SCTLR_Value.SA := SP_EL1_Alignment_Check_Enabled;
      Set_SCTLR_EL1 (SCTLR_Value);
      Strong_Memory_Barrier;
      CPU.Interrupt_Handling.Restore_Cpu_Interrupting (Old_Cpu_Interrupting_State);
   end Enable_MMU;

   procedure Disable_MMU is
      SCTLR_Value : SCTLR_EL1_Type;
      Old_Cpu_Interrupting_State : constant Cpu_Register_Type :=
         Interrupt_Handling.Disable_Cpu_Interrupting;
   begin
      Strong_Memory_Barrier;
      SCTLR_Value := Get_SCTLR_EL1;
      SCTLR_Value.M := MMU_Disabled;
      Set_SCTLR_EL1 (SCTLR_Value);
      Strong_Memory_Barrier;
      Invalidate_TLB;
      CPU.Interrupt_Handling.Restore_Cpu_Interrupting (Old_Cpu_Interrupting_State);
   end Disable_MMU;

   function Get_SCTLR_EL1 return SCTLR_EL1_Type is
      SCTLR_EL1_Value : SCTLR_EL1_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, sctlr_el1",
         Outputs => SCTLR_EL1_Type'Asm_Output ("=r", SCTLR_EL1_Value), --  %0
         Volatile => True);

      return SCTLR_EL1_Value;
   end Get_SCTLR_EL1;

   procedure Set_SCTLR_EL1 (SCTLR_EL1_Value : SCTLR_EL1_Type) is
   begin
      System.Machine_Code.Asm (
         "msr sctlr_el1, %0",
         Inputs => SCTLR_EL1_Type'Asm_Input ("r", SCTLR_EL1_Value), --  %0
         Volatile => True);
   end Set_SCTLR_EL1;

end CPU.Memory_Protection;
