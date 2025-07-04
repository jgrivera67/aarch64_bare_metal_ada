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
with CPU.Caches;
with CPU.Interrupt_Handling;
with CPU.Multicore;
with Utils.Runtime_Log;
with System.Machine_Code;

package body CPU.Memory_Protection is
   use Utils.Runtime_Log;

   procedure Initialize is
      Cpu_Id : constant Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
   begin
      if CPU.Caches_Are_Enabled then
         CPU.Caches.Disable_Caches;
      end if;

      if Mmu_Is_Enabled then
         Disable_MMU;
      end if;

      Initialize_MMU;

      Log_Debug_Msg ("Configuring MMU translation tables ...");

      --
      --  Configure global text region:
      --
      Configure_Memory_Region (Linker_Memory_Map.Global_Text_Region_Start_Address,
                               Linker_Memory_Map.Global_Text_Region_End_Address,
                               Unprivileged_Permissions => Read_Execute,
                               Privileged_Permissions => Read_Execute,
                               Region_Attributes =>
                                 --  Only reads need to be cached
                                 Normal_Memory_Write_Back_Cacheable);

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
            --  NOTE: We cannot change the contents of page 0 as that would overwrite
            --  the spin addresses to start the secondary CPUs.
            --  Null_Page := [others => Interfaces.Unsigned_64'Last];
            Configure_Memory_Region (Start_Virtual_Address => System.Null_Address,
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
                                 Normal_Memory_Write_Back_Cacheable);

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
      Configure_Memory_Region (Translation_Tables (Cpu_Id)'Address,
                               Translation_Tables_Array_Type'Object_Size / System.Storage_Unit,
                               Unprivileged_Permissions => None,
                               Privileged_Permissions => Read_Write,
                               Region_Attributes => Normal_Memory_Write_Back_Cacheable);

      Enable_MMU;
      CPU.Caches.Enable_Caches;
   end Initialize;

   procedure Initialize_MMU is
      procedure Load_Memory_Attributes_Lookup_Table is
         MAIR_Value : MAIR_Type;
      begin
         --
         --  Load all memory attributes supported into MAIR_EL1 register:
         --
         MAIR_Value.Attr_Array := Memory_Attributes_Lookup_Table;
         Set_MAIR (MAIR_Value);
      end Load_Memory_Attributes_Lookup_Table;

      TCR_Value : TCR_Type;
      Cpu_Id : constant Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Translation_Table_Tree : Translation_Table_Tree_Type renames Translation_Table_Trees (Cpu_Id);
   begin
      pragma Assert (Level3_Translation_Table_Entry_Range_Size = Page_Size_In_Bytes);
      pragma Assert (
         Virtual_Address_Space_Size_In_Bytes <=
            Max_Num_Translation_Table_Entries * Level1_Translation_Table_Entry_Range_Size);
      pragma Assert (
         Virtual_Address_Space_Size_In_Bytes mod Level1_Translation_Table_Entry_Range_Size = 0);

      Initialize_Translation_Table_Tree (Translation_Table_Tree,
                                         Translation_Tables (Cpu_Id)'Access);
      Load_Memory_Attributes_Lookup_Table;

      --
      --  Configure translation regime:
      --  - Normal memory, Inner Non-cacheable and Outer Non-cacheable for
      --    translation table memory
      --  - Non-Sharable translation table memory
      --  - 4KB granule size
      --  - Only TTBR0 will be used
      --
      TCR_Value.Value := 0;
      TCR_Value.T0SZ := Virtual_Address_Space_Size_TnSZ_Value;
      TCR_Value.IRGN0 := TT_Normal_Memory_Write_Back_Read_Allocate_Write_Allocate_Cacheable;
      TCR_Value.ORGN0 := TT_Normal_Memory_Write_Back_Read_Allocate_Write_Allocate_Cacheable;
      TCR_Value.SH0 := Inner_Shareable;
      TCR_Value.TG0 := TTBR0_Granule;
      TCR_Value.EPD1 := TTBR1_Translation_Table_Walk_Disabled;

      TCR_Value.T1SZ := Virtual_Address_Space_Size_TnSZ_Value;
      TCR_Value.IRGN1 := TT_Normal_Memory_Write_Back_Read_Allocate_Write_Allocate_Cacheable;
      TCR_Value.ORGN1 := TT_Normal_Memory_Write_Back_Read_Allocate_Write_Allocate_Cacheable;
      TCR_Value.SH1 := Inner_Shareable;
      TCR_Value.TG1 := TG1_4KB;
      TCR_Value.IPS := IPS_40_Bits;
      TCR_Value.HA := Hardware_Access_Flag_Update_Enabled;
      Set_TCR (TCR_Value);

      --
      --  Set TTBR0 to point to the level1 translation table:
      --
      declare
         L1_Table_Id : constant Translation_Table_Id_Type :=
            Translation_Table_Tree.Level1_Translation_Table_Id;
         L1_Table : Translation_Table_Type renames
            Translation_Table_Tree.Tables_Pointer.all (L1_Table_Id);
         TTBR0_Value : TTBRn_Type;
      begin
         pragma Assert (Address_Is_Page_Aligned (L1_Table'Address));
         TTBR0_Value.Value := Interfaces.Unsigned_64 (To_Integer (L1_Table'Address));
         Set_TTBR0 (TTBR0_Value);
      end;
   end Initialize_MMU;

   procedure Initialize_Translation_Table_Tree (
      Translation_Table_Tree : out Translation_Table_Tree_Type;
      Translation_Tables_Pointer : Translation_Tables_Array_Pointer_Type) is
      L1_Table_Id : Translation_Table_Id_Type;
   begin
      Translation_Table_Tree.Tables_Pointer := Translation_Tables_Pointer;
      Translation_Table_Tree.Next_Free_Table_Index := Valid_Translation_Table_Id_Type'First;
      Allocate_Translation_Table (Translation_Table_Tree, L1_Table_Id);
      Translation_Table_Tree.Level1_Translation_Table_Id := L1_Table_Id;
   end Initialize_Translation_Table_Tree;

   procedure Configure_Memory_Region (
      Start_Virtual_Address : System.Address;
      Size_In_Bytes : Integer_Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
      with SPARK_Mode => On
   is
      End_Virtual_Address : constant System.Address := To_Address (
         To_Integer (Start_Virtual_Address) + Size_In_Bytes);
   begin
      Configure_Memory_Region (Start_Virtual_Address,
                               End_Virtual_Address,
                               Unprivileged_Permissions,
                               Privileged_Permissions,
                               Region_Attributes);
   end Configure_Memory_Region;

   procedure Configure_Memory_Region (
      Start_Virtual_Address : System.Address;
      End_Virtual_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      Translation_Table_Tree : Translation_Table_Tree_Type renames
         Translation_Table_Trees (CPU.Multicore.Get_Cpu_Id);
   begin
      Populate_Level1_Translation_Table (Translation_Table_Tree,
                                         Start_Virtual_Address,
                                         End_Virtual_Address,
                                         Unprivileged_Permissions,
                                         Privileged_Permissions,
                                         Region_Attributes);
      if Region_Attributes = Normal_Memory_Write_Back_Cacheable or else
         Region_Attributes = Normal_Memory_Write_Through_Cacheable
      then
         if CPU.Caches_Are_Enabled then
            CPU.Caches.Flush_Invalidate_Data_Cache_Range (Start_Virtual_Address, End_Virtual_Address);
         else
            CPU.Caches.Invalidate_Data_Cache_Range (Start_Virtual_Address, End_Virtual_Address);
         end if;
      end if;
   end Configure_Memory_Region;

   procedure Populate_Level1_Translation_Table (
      Translation_Table_Tree : in out Translation_Table_Tree_Type;
      Start_Virtual_Address : System.Address;
      End_Virtual_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      L1_First_Index : constant Translation_Table_Entry_Index_Type :=
         Address_To_Level1_Table_Index (Start_Virtual_Address);
      Last_Virtual_Address : constant System.Address :=
         To_Address (To_Integer (End_Virtual_Address) - 1);
      L1_Last_Index : constant Translation_Table_Entry_Index_Type :=
         Address_To_Level1_Table_Index (Last_Virtual_Address);
      L1_Table_Id : constant Translation_Table_Id_Type :=
         Translation_Table_Tree.Level1_Translation_Table_Id;
      L1_Table : Translation_Table_Type renames
         Translation_Table_Tree.Tables_Pointer.all (L1_Table_Id);
   begin
      for L1_Index in L1_First_Index .. L1_Last_Index loop
         declare
            L1_Start_Virtual_Address : constant System.Address :=
               (if L1_Index > L1_First_Index then
                   Level1_Table_Index_To_Base_Virtual_Address (L1_Index)
                else
                  Start_Virtual_Address);
            L1_End_Virtual_Address : constant System.Address :=
               (if L1_Index < L1_Last_Index then
                   Level1_Table_Index_To_Base_Virtual_Address (L1_Index + 1)
                else
                   End_Virtual_Address);
         begin
            pragma Loop_Invariant (
               L1_Start_Virtual_Address < L1_End_Virtual_Address and then
               L1_Start_Virtual_Address >= Start_Virtual_Address and then
               L1_End_Virtual_Address <= End_Virtual_Address and then
               (if L1_Start_Virtual_Address > Start_Virtual_Address then
                   Address_Is_Aligned_To_Level1_Table_Entry_Range (L1_Start_Virtual_Address)) and then
               (if L1_End_Virtual_Address < End_Virtual_Address then
                   Address_Is_Aligned_To_Level1_Table_Entry_Range (L1_End_Virtual_Address)));

            Populate_Level1_Translation_Table_Entry (
               Translation_Table_Tree,
               L1_Table (L1_Index),
               L1_Start_Virtual_Address,
               L1_End_Virtual_Address,
               Unprivileged_Permissions,
               Privileged_Permissions,
               Region_Attributes);
         end;
      end loop;
   end Populate_Level1_Translation_Table;

   procedure Populate_Level1_Translation_Table_Entry (
      Translation_Table_Tree : in out Translation_Table_Tree_Type;
      L1_Translation_Table_Entry : out Translation_Table_Entry_Type;
      Start_Virtual_Address : System.Address;
      End_Virtual_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      L2_Table_Id : Translation_Table_Id_Type;
   begin
      if L1_Translation_Table_Entry.Valid_Entry = 1 then
         if L1_Translation_Table_Entry.Entry_Kind = Translation_Table_Entry_Is_Block then
            raise Program_Error with "Cannot overwrite existing entry";
         end if;

         declare
            L2_Table_Address : constant System.Address :=
               Page_Address_Prefix_To_Address (L1_Translation_Table_Entry.Page_Address_Prefix);
            L2_Table : Translation_Table_Type with Import, Address => L2_Table_Address;
         begin
            Populate_Level2_Translation_Table (
               Translation_Table_Tree,
               L2_Table,
               Start_Virtual_Address,
               End_Virtual_Address,
               Unprivileged_Permissions,
               Privileged_Permissions,
               Region_Attributes);
         end;
      else
         if Address_Is_Aligned_To_Level1_Table_Entry_Range (Start_Virtual_Address) and then
            Address_Is_Aligned_To_Level1_Table_Entry_Range (End_Virtual_Address)
         then
            --
            --  Block of memory is aligned to level 1 translation table entry size
            --  (e.g. 1GB for 4KB page granule size)
            --
            Populate_Translation_Table_Leaf_Entry (
               L1_Translation_Table_Entry,
               Start_Virtual_Address,
               Unprivileged_Permissions,
               Privileged_Permissions,
               Region_Attributes,
               TT_Level1);
         else
            Allocate_Translation_Table (Translation_Table_Tree, L2_Table_Id);
            declare
               L2_Table : Translation_Table_Type renames
                  Translation_Table_Tree.Tables_Pointer.all (L2_Table_Id);
            begin
               Populate_Level2_Translation_Table (
                  Translation_Table_Tree,
                  L2_Table,
                  Start_Virtual_Address,
                  End_Virtual_Address,
                  Unprivileged_Permissions,
                  Privileged_Permissions,
                  Region_Attributes);

               Populate_Translation_Table_Inner_Entry (
                  L1_Translation_Table_Entry,
                  L2_Table'Address);
            end;
         end if;
      end if;
   end Populate_Level1_Translation_Table_Entry;

   procedure Populate_Level2_Translation_Table (
      Translation_Table_Tree : in out Translation_Table_Tree_Type;
      L2_Translation_Table : out Translation_Table_Type;
      Start_Virtual_Address : System.Address;
      End_Virtual_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      L1_Index : constant Translation_Table_Entry_Index_Type :=
         Address_To_Level1_Table_Index (Start_Virtual_Address);
      L2_First_Index : constant Translation_Table_Entry_Index_Type :=
         Address_To_Level2_Table_Index (Start_Virtual_Address);
      Last_Virtual_Address : constant System.Address :=
         To_Address (To_Integer (End_Virtual_Address) - 1);
      L2_Last_Index : constant Translation_Table_Entry_Index_Type :=
         Address_To_Level2_Table_Index (Last_Virtual_Address);
   begin
      for L2_Index in L2_First_Index .. L2_Last_Index loop
         declare
            L2_Start_Virtual_Address : constant System.Address :=
               (if L2_Index > L2_First_Index then
                   Level2_Table_Index_To_Base_Virtual_Address (L1_Index, L2_Index)
                else
                  Start_Virtual_Address);
            L2_End_Virtual_Address : constant System.Address :=
               (if L2_Index < L2_Last_Index then
                   Level2_Table_Index_To_Base_Virtual_Address (L1_Index, L2_Index + 1)
                else
                   End_Virtual_Address);
         begin
            pragma Loop_Invariant (
               L2_Start_Virtual_Address < L2_End_Virtual_Address and then
               L2_Start_Virtual_Address >= Start_Virtual_Address and then
               L2_End_Virtual_Address <= End_Virtual_Address and then
               (if L2_Start_Virtual_Address > Start_Virtual_Address then
                   Address_Is_Aligned_To_Level2_Table_Entry_Range (L2_Start_Virtual_Address)) and then
               (if L2_End_Virtual_Address < End_Virtual_Address then
                   Address_Is_Aligned_To_Level2_Table_Entry_Range (L2_End_Virtual_Address)));

            Populate_Level2_Translation_Table_Entry (
               Translation_Table_Tree,
               L2_Translation_Table (L2_Index),
               L2_Start_Virtual_Address,
               L2_End_Virtual_Address,
               Unprivileged_Permissions,
               Privileged_Permissions,
               Region_Attributes);
         end;
      end loop;
   end Populate_Level2_Translation_Table;

   procedure Populate_Level2_Translation_Table_Entry (
      Translation_Table_Tree : in out Translation_Table_Tree_Type;
      L2_Translation_Table_Entry : out Translation_Table_Entry_Type;
      Start_Virtual_Address : System.Address;
      End_Virtual_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      L3_Table_Id : Translation_Table_Id_Type;
   begin
      if L2_Translation_Table_Entry.Valid_Entry = 1 then
         if L2_Translation_Table_Entry.Entry_Kind = Translation_Table_Entry_Is_Block then
            raise Program_Error with "Cannot overwrite existing entry";
         end if;

         declare
            L3_Table_Address : constant System.Address :=
               Page_Address_Prefix_To_Address (L2_Translation_Table_Entry.Page_Address_Prefix);
            L3_Table : Translation_Table_Type with Import, Address => L3_Table_Address;
         begin
            Populate_Level3_Translation_Table (
               L3_Table,
               Start_Virtual_Address,
               End_Virtual_Address,
               Unprivileged_Permissions,
               Privileged_Permissions,
               Region_Attributes);
         end;
      else
         if Address_Is_Aligned_To_Level2_Table_Entry_Range (Start_Virtual_Address) and then
            Address_Is_Aligned_To_Level2_Table_Entry_Range (End_Virtual_Address)
         then
            --
            --  Block of memory is aligned to level 2 translation table entry size
            --  (e.g. 2MB for 4KB page granule size)
            --
            Populate_Translation_Table_Leaf_Entry (
               L2_Translation_Table_Entry,
               Start_Virtual_Address,
               Unprivileged_Permissions,
               Privileged_Permissions,
               Region_Attributes,
               TT_Level2);
         else
            Allocate_Translation_Table (Translation_Table_Tree, L3_Table_Id);
            declare
               L3_Table : Translation_Table_Type renames
                  Translation_Table_Tree.Tables_Pointer.all (L3_Table_Id);
            begin
               Populate_Level3_Translation_Table (
                  L3_Table,
                  Start_Virtual_Address,
                  End_Virtual_Address,
                  Unprivileged_Permissions,
                  Privileged_Permissions,
                  Region_Attributes);

               Populate_Translation_Table_Inner_Entry (
                  L2_Translation_Table_Entry,
                  L3_Table'Address);
            end;
         end if;
      end if;
   end Populate_Level2_Translation_Table_Entry;

   procedure Populate_Level3_Translation_Table (
      L3_Translation_Table : out Translation_Table_Type;
      Start_Virtual_Address : System.Address;
      End_Virtual_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type)
   is
      L3_First_Index : constant Translation_Table_Entry_Index_Type :=
         Address_To_Level3_Table_Index (Start_Virtual_Address);
      Last_Virtual_Address : constant System.Address :=
         To_Address (To_Integer (End_Virtual_Address) - 1);
      L3_Last_Index : constant Translation_Table_Entry_Index_Type :=
         Address_To_Level3_Table_Index (Last_Virtual_Address);
      L3_Start_Virtual_Address : System.Address := Start_Virtual_Address;
   begin
      for L3_Index in L3_First_Index .. L3_Last_Index loop
         pragma Loop_Invariant (L3_Start_Virtual_Address >= Start_Virtual_Address and then
                                L3_Start_Virtual_Address < End_Virtual_Address);
         Populate_Translation_Table_Leaf_Entry (
            L3_Translation_Table (L3_Index),
            L3_Start_Virtual_Address,
            Unprivileged_Permissions,
            Privileged_Permissions,
            Region_Attributes,
            TT_Level3);

         L3_Start_Virtual_Address :=
            To_Address (To_Integer (@) + Level3_Translation_Table_Entry_Range_Size);
      end loop;
   end Populate_Level3_Translation_Table;

   procedure Populate_Translation_Table_Inner_Entry (
      Translation_Table_Entry : out Translation_Table_Entry_Type;
      Child_Translation_Table_Address : System.Address)
   is
   begin
      Translation_Table_Entry.Page_Address_Prefix :=
         Address_To_Page_Address_Prefix (Child_Translation_Table_Address);
      Translation_Table_Entry.Entry_Kind := Translation_Table_Entry_Is_Table_Or_Page;
      Translation_Table_Entry.Valid_Entry := 1;
   end Populate_Translation_Table_Inner_Entry;

   procedure Populate_Translation_Table_Leaf_Entry (
      Translation_Table_Entry : out Translation_Table_Entry_Type;
      Start_Physical_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Region_Attributes : Region_Attributes_Type;
      Translation_Table_Level : Translation_Table_Level_Type)
   is
      Entry_Kind : constant Translation_Table_Entry_Kind_Type :=
         (if Translation_Table_Level = TT_Level3 then
            Translation_Table_Entry_Is_Table_Or_Page
          else
            Translation_Table_Entry_Is_Block);
   begin
      Translation_Table_Entry.SH := Inner_Shareable;
      Translation_Table_Entry.PXN := Non_Executable;
      Translation_Table_Entry.UXN := Non_Executable;
      case Privileged_Permissions is
         when Read_Write =>
            if Unprivileged_Permissions = Read_Write then
               Translation_Table_Entry.AP := EL1_and_EL0_Read_Write;
            else
               pragma Assert (Unprivileged_Permissions = None);
               Translation_Table_Entry.AP := EL1_Read_Write_EL0_No_Access;
            end if;
         when Read_Only =>
            if Unprivileged_Permissions = Read_Only then
               Translation_Table_Entry.AP := EL1_and_EL0_Read_Only;
            else
               pragma Assert (Unprivileged_Permissions = None);
               Translation_Table_Entry.AP := EL1_Read_Only_EL0_No_Access;
            end if;
         when Read_Execute =>
            Translation_Table_Entry.PXN := Executable;
            if Unprivileged_Permissions = Read_Execute then
               Translation_Table_Entry.UXN := Executable;
               Translation_Table_Entry.AP := EL1_and_EL0_Read_Only;
            else
               pragma Assert (Unprivileged_Permissions = None);
               Translation_Table_Entry.AP := EL1_Read_Only_EL0_No_Access;
            end if;
         when Read_Write_Execute =>
            pragma Assert (Unprivileged_Permissions = Read_Write_Execute);
            Translation_Table_Entry.PXN := Executable;
            Translation_Table_Entry.UXN := Executable;
            Translation_Table_Entry.AP := EL1_and_EL0_Read_Write;
         when None =>
            null;
      end case;

      Translation_Table_Entry.Page_Address_Prefix :=
         Address_To_Page_Address_Prefix (Start_Physical_Address);
      Translation_Table_Entry.Attr_Index :=
         Translation_Table_MAIR_EL1_Index_Type (Region_Attributes'Enum_Rep);
      Translation_Table_Entry.AF := 1;
      Translation_Table_Entry.nG := 0;
      Translation_Table_Entry.NS := 1;
      Translation_Table_Entry.Entry_Kind := Entry_Kind;
      Translation_Table_Entry.Valid_Entry := 1;

      if Debug_On then
         Print_Translation_Table_Leaf_Entry (Translation_Table_Entry,
                                             Start_Physical_Address,
                                             Start_Physical_Address,
                                             Unprivileged_Permissions,
                                             Privileged_Permissions,
                                             Region_Attributes,
                                             Translation_Table_Level);
      end if;
   end Populate_Translation_Table_Leaf_Entry;

   procedure Print_Translation_Table_Leaf_Entry (
      Translation_Table_Entry : Translation_Table_Entry_Type;
      Start_Virtual_Address : System.Address;
      Start_Physical_Address : System.Address;
      Unprivileged_Permissions : Region_Permissions_Type;
      Privileged_Permissions : Region_Permissions_Type;
      Caching_Attributes : Region_Attributes_Type;
      Translation_Table_Level : Translation_Table_Level_Type)
   is
      procedure Print_Permissions (Permissions : Region_Permissions_Type)
      is
      begin
         case Permissions is
            when Read_Write =>
               Log_Debug_Msg_Part ("rw-");
            when Read_Only =>
               Log_Debug_Msg_Part ("r--");
            when Read_Execute =>
               Log_Debug_Msg_Part ("r-x");
            when Read_Write_Execute =>
               Log_Debug_Msg_Part ("rwx");
            when None =>
               Log_Debug_Msg_Part ("---");
         end case;
      end Print_Permissions;

      procedure Print_Caching_Attributes (Caching_Attributes : Region_Attributes_Type)
      is
      begin
         case Caching_Attributes is
            when Device_Memory_Mapped_Io =>
               Log_Debug_Msg_Part ("device-memory");
            when Normal_Memory_Non_Cacheable =>
               Log_Debug_Msg_Part ("normal-memory-non-cacheable");
            when Normal_Memory_Write_Through_Cacheable =>
               Log_Debug_Msg_Part ("normal-memory-write-through-cacheable");
            when Normal_Memory_Write_Back_Cacheable =>
               Log_Debug_Msg_Part ("normal-memory-write-back-cacheable");
         end case;
      end Print_Caching_Attributes;
   begin
      Log_Debug_Msg_Begin ("Level ");
      Log_Debug_Value_Decimal (Translation_Table_Level'Enum_Rep);
      Log_Debug_Msg_Part (" translation (");
      Log_Debug_Value_Hexadecimal (Translation_Table_Entry.Value);
      Log_Debug_Msg_Part (")");
      if Start_Virtual_Address = Start_Physical_Address then
         Log_Debug_Msg_Part (" VA=PA=");
         Log_Debug_Value_Hexadecimal (
            Interfaces.Unsigned_64 (To_Integer (Start_Physical_Address)));
      else
         Log_Debug_Msg_Part (" VA=");
         Log_Debug_Value_Hexadecimal (
            Interfaces.Unsigned_64 (To_Integer (Start_Virtual_Address)));
         Log_Debug_Msg_Part (" PA=");
         Log_Debug_Value_Hexadecimal (
            Interfaces.Unsigned_64 (To_Integer (Start_Physical_Address)));
      end if;

      Log_Debug_Msg_Part (" Privileged Perms=");
      Print_Permissions (Privileged_Permissions);
      Log_Debug_Msg_Part (" Unprivileged Perms=");
      Print_Permissions (Unprivileged_Permissions);
      Log_Debug_Msg_Part (" Caching Attrs=");
      Print_Caching_Attributes (Caching_Attributes);
      Log_Debug_Msg_End;
   end Print_Translation_Table_Leaf_Entry;

   procedure Allocate_Translation_Table (
      Translation_Table_Tree : in out Translation_Table_Tree_Type;
      Translation_Table_Id : out Valid_Translation_Table_Id_Type) is
      Old_Cpu_Interrupting_State : constant Cpu_Register_Type :=
         Interrupt_Handling.Disable_Cpu_Interrupting;
   begin
      if Translation_Table_Tree.Next_Free_Table_Index = Translation_Table_Id_Type'Last then
         raise Program_Error with "No more translation tables available";
      end if;

      Translation_Table_Id := Translation_Table_Tree.Next_Free_Table_Index;
      Translation_Table_Tree.Next_Free_Table_Index := @ + 1;

      Translation_Table_Tree.Tables_Pointer.all (Translation_Table_Id) := [others => <>];
      Interrupt_Handling.Restore_Cpu_Interrupting (Old_Cpu_Interrupting_State);
   end Allocate_Translation_Table;

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
         Inputs => Interfaces.Unsigned_64'Asm_Input ("r", TCR_Value.Value), --  %0
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
         "tlbi vmalle1is" & ASCII.LF &
         "dsb ish" & ASCII.LF &
         "isb",
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
      SCTLR_Value.SA0 := SP_EL0_Alignment_Check_Enabled;
      --  NOTE: We don't set SA, because we never initialize SP_EL1
      --  SCTLR_Value.SA := SP_EL1_Alignment_Check_Enabled;

      --
      --  NOTE: SPAN needs to be disabled, otherwise exceptions will not work on Cortex-A76.
      --  Cortex-A72 (ARMv8.0-A) does not support PAN (Privileged Access Never), but
      --  cortex-A76 (ARMv8.2-A) does and SCTLR_EL1.SPAN is enabled by default.
      --  When SPAN is enabled, PSTATE.PAN is set when entering an EL1 exception.
      --  When PSTATE.PAN is enabled, accessing any memory at that is accessible from EL0,
      --  while running at EL1, will cause memory access fault.
      --  Since the ISR stack is accessible from EL0 (to support going to EL0 in the reset
      --  handler), having SPAN enabled will cause the ISR stack not being accessible
      --  when taking EL1 exceptions.
      --
      SCTLR_Value.SPAN := Set_Privileged_Access_Never_Disabled;
      Set_SCTLR_EL1 (SCTLR_Value);
      Strong_Memory_Barrier;
      Log_Info_Msg ("MMU enabled");
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
      Log_Info_Msg ("MMU disabled");
      CPU.Interrupt_Handling.Restore_Cpu_Interrupting (Old_Cpu_Interrupting_State);
   end Disable_MMU;

end CPU.Memory_Protection;
