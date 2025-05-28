--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Interrupt controller driver for ARM GIC-400 (GICv2)

with CPU;
with CPU.Interrupt_Handling;

package body Interrupt_Controller_Driver with
  SPARK_Mode => Off
is
   ----------------------------------------------------------------------------
   --  Public Subprograms
   ----------------------------------------------------------------------------

   procedure Initialize with
     Refined_Post => Interrupt_Controller_Obj.GIC_Distributor_Initialized
   is
      procedure Initialize_GIC_Distributor with
        Pre  => not Interrupt_Controller_Obj.GIC_Distributor_Initialized,
        Post => Interrupt_Controller_Obj.GIC_Distributor_Initialized;

      procedure Initialize_GIC_Distributor is
         use type Interfaces.Unsigned_16;
         GICD_CTLR_Value              : GICD_CTLR_Type;
         GICD_TYPER_Value             : GICD_TYPER_Type;
         Max_Number_Interrupt_Sources : Interfaces.Unsigned_16;
      begin
         --  Disable interrupts from group0 and group1 before configuring the GIC:
         GICD_CTLR_Value            := GICD.GICD_CTLR;
         GICD_CTLR_Value.EnableGrp0 := Group_Interrupts_Disabled;
         GICD_CTLR_Value.EnableGrp1 := Group_Interrupts_Disabled;
         GICD.GICD_CTLR             := GICD_CTLR_Value;
         CPU.Strong_Memory_Barrier;

         GICD_TYPER_Value := GICD.GICD_TYPER;
         if GICD_TYPER_Value.ITLinesNumber /= 0 then
            Max_Number_Interrupt_Sources :=
               32 * (Interfaces.Unsigned_16 (GICD_TYPER_Value.ITLinesNumber) + 1) - 1;
            pragma Assert (
               Max_Number_Interrupt_Sources <= Interfaces.Unsigned_16 (Max_Num_Interrupts_Supported));
         else
            Utils.Print_String ("*** GICD_TYPER.ITLinesNumber is 0" & ASCII.LF); --???
            Max_Number_Interrupt_Sources := Max_Num_Interrupts_Supported;
         end if;

         --
         --  Disable and clear all interrupts:
         --
         for I in GICD_ICENABLER_Array_Type'Range loop
            GICD.GICD_ICENABLER_Array (I) := [others => 2#1#];
            GICD.GICD_ICPENDR_Array (I) := [others => 2#1#];
            GICD.GICD_ICACTIVER_Array (I) := [others => 2#1#];
         end loop;
         CPU.Strong_Memory_Barrier;

         --
         --  Enable reception of interrupts from group0/group1 in the GIC:
         --
         --  NOTE: Interrupt group 1 will be used for regular interrupts
         --  as they are routed to the IRQ interrupt line of the CPU core.
         --  Interrupt group 0 will be used for high-priority non-maskable
         --  interrupts, as they are routed to the FIQ interrupt line of
         --  the CPU core.
         --
         GICD_CTLR_Value            := GICD.GICD_CTLR;
         GICD_CTLR_Value.EnableGrp0 := Group_Interrupts_Enabled; --  FIQ
         GICD_CTLR_Value.EnableGrp1 := Group_Interrupts_Enabled; --  IRQ
         GICD.GICD_CTLR             := GICD_CTLR_Value;
         CPU.Strong_Memory_Barrier;

         Interrupt_Controller_Obj.Max_Number_Interrupt_Sources :=
           Max_Number_Interrupt_Sources;
         Interrupt_Controller_Obj.GIC_Distributor_Initialized  := True;
         CPU.Multicore.Send_Multicore_Event;
      end Initialize_GIC_Distributor;

      procedure Initialize_GIC_Cpu_Interface is
         ICC_CTLR_Value   : ICC_CTLR_Type;
         ICC_BPR_Value    : ICC_BPR_Type;
         ICC_PMR_Value    : ICC_PMR_Type;
      begin
         --  Enable CPU interface:
         ICC_CTLR_Value := GICC.ICC_CTLR;
         ICC_CTLR_Value.CBPR := Use_ICC_BPR0_For_Interrupt_Preemption_Enabled;
         ICC_CTLR_Value.EOImode :=  ICC_EOIRx_Write_Deactives_Interrupt_Enabled;
         GICC.ICC_CTLR := ICC_CTLR_Value;

         --
         --  Set binary point to maximize interrupt preemptability for nested interrupts,
         --  for groups 0 (FIQ) and 1 (IRQ):
         --
         ICC_BPR_Value.Binary_Point := Binary_Point_Type'First;
         GICC.ICC_BPR := ICC_BPR_Value;

         --
         --  Set current interrupt priority mask to accept all interrupt priorities
         --  supported:
         --
         --  NOTE: Interrupt_Priority_Type'Last is the lowest priority supported.
         --  Interrupt_Priority_Type'First is the highest priority.
         --  The GIC only signals pending interrupts with a higher priority (lower
         --  priority value) than the value set in ICC_PMR.
         --
         ICC_PMR_Value.Priority := GIC_Interrupt_Priority_Type'Last;
         GICC.ICC_PMR := ICC_PMR_Value;
      end Initialize_GIC_Cpu_Interface;

      use type CPU.Valid_Cpu_Core_Id_Type;
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Old_Flags : CPU.Cpu_Register_Type with Unreferenced;

   begin
      if Cpu_Id = CPU.Valid_Cpu_Core_Id_Type'First then
         pragma Assert (not Per_Cpu_Initialized);
         Initialize_GIC_Distributor;
      else
         while not Interrupt_Controller_Obj.GIC_Distributor_Initialized loop
            CPU.Multicore.Wait_For_Multicore_Event;
         end loop;
      end if;

      Initialize_GIC_Cpu_Interface;

      Old_Flags :=
         CPU.Multicore.Atomic_Fetch_Or (Interrupt_Controller_Obj.Per_Cpu_Initialized_Flags,
                                        Bit_Mask (Bit_Index_Type (Cpu_Id)));
   end Initialize;

   procedure Configure_Internal_Interrupt
     (Internal_Interrupt_Id         : Internal_Interrupt_Id_Type;
      Priority                      : Valid_Interrupt_Priority_Type with Unreferenced;
      Cpu_Interrupt_Line            : Cpu_Interrupt_Line_Type with Unreferenced;
      Trigger_Mode                  : Interrupt_Trigger_Mode_Type with Unreferenced;
      Interrupt_Handler_Entry_Point : Interrupt_Handler_Entry_Point_Type;
      Interrupt_Handler_Arg         : System.Address := System.Null_Address)
   is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Interrupt_Handler : Interrupt_Handler_Type renames
        Interrupt_Controller_Obj.Internal_Interrupt_Handlers (Cpu_Id, Internal_Interrupt_Id);
      IPRIORITY_Register_Index : constant Eight_Bits_Per_Interrupt_Register_Index_Type :=
        Eight_Bits_Per_Interrupt_Register_Index_Type (Internal_Interrupt_Id /
                                                      GIC_IPRIORITYR_Slot_Array_Type'Length);
      IPRIORITY_Field_Index    : constant Eight_Bits_Per_Interrupt_Field_Index_Type :=
        Eight_Bits_Per_Interrupt_Field_Index_Type (Internal_Interrupt_Id mod
                                                   GIC_IPRIORITYR_Slot_Array_Type'Length);
      IGROUPR_Register_Index    : constant One_Bit_Per_Interrupt_Register_Index_Type :=
         One_Bit_Per_Interrupt_Register_Index_Type (Internal_Interrupt_Id / GIC_IGROUPR_Type'Length);
      IGROUPR_Field_Index       : constant One_Bit_Per_Interrupt_Field_Index_Type :=
         One_Bit_Per_Interrupt_Field_Index_Type (Internal_Interrupt_Id mod
                                                 GIC_IGROUPR_Type'Length);
      GIC_IPRIORITYR_Value     : GIC_IPRIORITYR_Type;
      GIC_IGROUPR_Value        : GIC_IGROUPR_Type;
   begin
      pragma Assert (Interrupt_Handler.Interrupt_Handler_Entry_Point = null);
      Interrupt_Handler.Cpu_Id                        := Cpu_Id;
      Interrupt_Handler.Interrupt_Handler_Entry_Point :=
        Interrupt_Handler_Entry_Point;
      Interrupt_Handler.Interrupt_Handler_Arg         := Interrupt_Handler_Arg;

      --
      --  NOTE: We do not need to serialize access to the GICD from multiple
      --  CPU cores, as since corresponding GICD registers for SGIs and PPIs are
      --  banked for each CPU core (for up to 8 cores).
      --

      --
      --  Configure interrupt trigger mode:
      --
      --  NOTE: For the GIC-400 (GICv2), all PPI signals are active-LOW
      --  level-sensitive.
      --

      --
      --  Configure interrupt priority:
      --
      GIC_IPRIORITYR_Value := GICD.GICD_IPRIORITYR_Array (IPRIORITY_Register_Index);
      GIC_IPRIORITYR_Value.Slot_Array (IPRIORITY_Field_Index).Interrupt_Priority :=
         GIC_Interrupt_Priority_Type (Priority);
      GICD.GICD_IPRIORITYR_Array (IPRIORITY_Register_Index) := GIC_IPRIORITYR_Value;

      --
      --  Assign interrupt to an interrupt group:
      --
      GIC_IGROUPR_Value := GICD.GICD_IGROUPR_Array (IGROUPR_Register_Index);
      GIC_IGROUPR_Value (IGROUPR_Field_Index) :=
         GIC_Interrupt_Group_Type (Cpu_Interrupt_Line);
      GICD.GICD_IGROUPR_Array (IGROUPR_Register_Index) := GIC_IGROUPR_Value;

      --
      --  NOTE: The interrupt starts to fire on the CPU only after Enable_Internal_Interrupt()
      --  is called.
      --
   end Configure_Internal_Interrupt;

   procedure Configure_External_Interrupt
     (External_Interrupt_Id         : External_Interrupt_Id_Type;
      Priority                      : Valid_Interrupt_Priority_Type;
      Cpu_Interrupt_Line            : Cpu_Interrupt_Line_Type;
      Trigger_Mode                  : Interrupt_Trigger_Mode_Type;
      Interrupt_Handler_Entry_Point : Interrupt_Handler_Entry_Point_Type;
      Interrupt_Handler_Arg         : System.Address := System.Null_Address)
   is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Interrupt_Handler : Interrupt_Handler_Type renames
         Interrupt_Controller_Obj.External_Interrupt_Handlers (External_Interrupt_Id);
      ICFGR_Register_Index : constant Two_Bits_Per_Interrupt_Register_Index_Type :=
         Two_Bits_Per_Interrupt_Register_Index_Type (External_Interrupt_Id /
                                                     GIC_ICFGR_Interrupt_Trigger_Mode_Array_Type'Length);
      ICFGR_Field_Index : constant Two_Bits_Per_Interrupt_Field_Index_Type :=
         Two_Bits_Per_Interrupt_Field_Index_Type (External_Interrupt_Id mod
                                                  GIC_ICFGR_Interrupt_Trigger_Mode_Array_Type'Length);
      IPRIORITYR_Register_Index : constant Eight_Bits_Per_Interrupt_Register_Index_Type :=
         Eight_Bits_Per_Interrupt_Register_Index_Type (External_Interrupt_Id /
                                                       GIC_IPRIORITYR_Slot_Array_Type'Length);
      IPRIORITYR_Field_Index    : constant Eight_Bits_Per_Interrupt_Field_Index_Type :=
         Eight_Bits_Per_Interrupt_Field_Index_Type (External_Interrupt_Id mod
                                                    GIC_IPRIORITYR_Slot_Array_Type'Length);
      IGROUPR_Register_Index    : constant One_Bit_Per_Interrupt_Register_Index_Type :=
         One_Bit_Per_Interrupt_Register_Index_Type (External_Interrupt_Id /
                                                    GIC_IGROUPR_Type'Length);
      IGROUPR_Field_Index       : constant One_Bit_Per_Interrupt_Field_Index_Type :=
         One_Bit_Per_Interrupt_Field_Index_Type (External_Interrupt_Id mod
                                                 GIC_IGROUPR_Type'Length);
      ITARGETSR_Register_Index : constant Eight_Bits_Per_Interrupt_Register_Index_Type :=
         Eight_Bits_Per_Interrupt_Register_Index_Type (External_Interrupt_Id /
                                                       GIC_ITARGETSR_Slot_Array_Type'Length);
      ITARGETSR_Field_Index    : constant Eight_Bits_Per_Interrupt_Field_Index_Type :=
         Eight_Bits_Per_Interrupt_Field_Index_Type (External_Interrupt_Id mod
                                                    GIC_ITARGETSR_Slot_Array_Type'Length);
      GIC_ICFGR_Value           : GIC_ICFGR_Type;
      GIC_IPRIORITYR_Value      : GIC_IPRIORITYR_Type;
      GIC_IGROUPR_Value         : GIC_IGROUPR_Type;
      GIC_ITARGETSR_Value      : GIC_ITARGETSR_Type;
   begin
      pragma Assert (Interrupt_Handler.Interrupt_Handler_Entry_Point = null);
      Interrupt_Handler.Cpu_Id                        := Cpu_Id;
      Interrupt_Handler.Interrupt_Handler_Entry_Point :=
        Interrupt_Handler_Entry_Point;
      Interrupt_Handler.Interrupt_Handler_Arg         := Interrupt_Handler_Arg;

      --
      --  NOTE: We need to serialize access to the GICD from multiple
      --  CPU cores, as external interrupts (shared peripheral interrupts)
      --  are not per CPU core.
      --
      CPU.Multicore.Spinlock_Acquire (Interrupt_Controller_Obj.Spinlock);

      --
      --  Configure interrupt trigger mode:
      --
      GIC_ICFGR_Value := GICD.GICD_ICFGR_Array (ICFGR_Register_Index);
      GIC_ICFGR_Value.Interrupt_Trigger_Mode_Array (ICFGR_Field_Index) :=
        GIC_ICFGR_Interrupt_Trigger_Mode_Type (Trigger_Mode);
      GICD.GICD_ICFGR_Array (ICFGR_Register_Index) := GIC_ICFGR_Value;

      --
      --  Configure interrupt priority:
      --
      GIC_IPRIORITYR_Value := GICD.GICD_IPRIORITYR_Array (IPRIORITYR_Register_Index);
      GIC_IPRIORITYR_Value.Slot_Array (IPRIORITYR_Field_Index).Interrupt_Priority :=
        GIC_Interrupt_Priority_Type (Priority);
      GICD.GICD_IPRIORITYR_Array (IPRIORITYR_Register_Index) := GIC_IPRIORITYR_Value;

      --
      --  Assign interrupt to interrupt group 0 (FIQ) or group 1 (IRQ):
      --
      GIC_IGROUPR_Value := GICD.GICD_IGROUPR_Array (IGROUPR_Register_Index);
      GIC_IGROUPR_Value (IGROUPR_Field_Index) :=
        GIC_Interrupt_Group_Type (Cpu_Interrupt_Line);
      GICD.GICD_IGROUPR_Array (IGROUPR_Register_Index) := GIC_IGROUPR_Value;

      --
      --  Route interrupt to target CPU:
      --
      GIC_ITARGETSR_Value := GICD.GICD_ITARGETSR_Array (ITARGETSR_Register_Index);
      GIC_ITARGETSR_Value.Slot_Array (ITARGETSR_Field_Index)(
         GIC_ITARGETSR_Slot_Cpu_Index_Type (Cpu_Id)) := 1;
      GICD.GICD_ITARGETSR_Array (ITARGETSR_Register_Index) := GIC_ITARGETSR_Value;

      CPU.Multicore.Spinlock_Release (Interrupt_Controller_Obj.Spinlock);

      --
      --  NOTE: The interrupt starts to fire on the CPU only after Enable_External_Interrupt()
      --  is called.
      --
   end Configure_External_Interrupt;

   procedure Enable_Internal_Interrupt (Internal_Interrupt_Id : Internal_Interrupt_Id_Type)
   is
      GIC_ISENABLER_Value      : GIC_ISENABLER_Type;
      ISENABLER_Register_Index : constant One_Bit_Per_Interrupt_Register_Index_Type :=
         One_Bit_Per_Interrupt_Register_Index_Type (Internal_Interrupt_Id /
                                                    GIC_ISENABLER_Type'Length);
      ISENABLER_Field_Index    : constant One_Bit_Per_Interrupt_Field_Index_Type :=
         One_Bit_Per_Interrupt_Field_Index_Type (Internal_Interrupt_Id mod
                                                 GIC_ISENABLER_Type'Length);
   begin
      --
      --  NOTE: We do not need to serialize access to the GICD from multiple
      --  CPU cores, when enabling SGIs/PPIs, as the corresponding GICD
      --  registers are banked for each CPU core.
      --
      GIC_ISENABLER_Value                                  := [others => 0];
      GIC_ISENABLER_Value (ISENABLER_Field_Index)          := 1;
      GICD.GICD_ISENABLER_Array (ISENABLER_Register_Index) := GIC_ISENABLER_Value;
   end Enable_Internal_Interrupt;

   procedure Enable_External_Interrupt (External_Interrupt_Id : External_Interrupt_Id_Type)
   is
      GIC_ISENABLER_Value      : GIC_ISENABLER_Type;
      ISENABLER_Register_Index : constant One_Bit_Per_Interrupt_Register_Index_Type :=
        One_Bit_Per_Interrupt_Register_Index_Type (External_Interrupt_Id /
                                                   GIC_ISENABLER_Type'Length);
      ISENABLER_Field_Index    : constant One_Bit_Per_Interrupt_Field_Index_Type :=
        One_Bit_Per_Interrupt_Field_Index_Type (External_Interrupt_Id mod
                                                GIC_ISENABLER_Type'Length);
   begin
      --
      --  NOTE: We need to serialize access to the GICD from multiple
      --  CPU cores, as external interrupts (shared peripherla interrupts)
      --  are not per CPU core.
      --
      CPU.Multicore.Spinlock_Acquire (Interrupt_Controller_Obj.Spinlock);
      GIC_ISENABLER_Value                                  := [others => 0];
      GIC_ISENABLER_Value (ISENABLER_Field_Index)          := 1;
      GICD.GICD_ISENABLER_Array (ISENABLER_Register_Index) := GIC_ISENABLER_Value;
      CPU.Multicore.Spinlock_Release (Interrupt_Controller_Obj.Spinlock);
   end Enable_External_Interrupt;

   procedure Disable_Internal_Interrupt (Internal_Interrupt_Id : Internal_Interrupt_Id_Type)
   is
      GIC_ICENABLER_Value      : GIC_ICENABLER_Type;
      ICENABLER_Register_Index : constant One_Bit_Per_Interrupt_Register_Index_Type :=
        One_Bit_Per_Interrupt_Register_Index_Type (Internal_Interrupt_Id /
                                                   GIC_ICENABLER_Type'Length);
      ICENABLER_Field_Index    : constant One_Bit_Per_Interrupt_Field_Index_Type :=
        One_Bit_Per_Interrupt_Field_Index_Type (Internal_Interrupt_Id mod
                                                GIC_ICENABLER_Type'Length);
   begin
      --
      --  NOTE: We do not need to serialize access to the GICD from multiple
      --  CPU cores, when disabling SGIs/PPIs, as the corresponding GICD
      --  registers are banked for each CPU core.
      --
      GIC_ICENABLER_Value                                  := [others => 0];
      GIC_ICENABLER_Value (ICENABLER_Field_Index)          := 1;
      GICD.GICD_ICENABLER_Array (ICENABLER_Register_Index) := GIC_ICENABLER_Value;
   end Disable_Internal_Interrupt;

   procedure Disable_External_Interrupt
     (External_Interrupt_Id : External_Interrupt_Id_Type)
   is
      GIC_ICENABLER_Value      : GIC_ICENABLER_Type;
      ICENABLER_Register_Index : constant One_Bit_Per_Interrupt_Register_Index_Type :=
        One_Bit_Per_Interrupt_Register_Index_Type (External_Interrupt_Id /
                                                   GIC_ICENABLER_Type'Length);
      ICENABLER_Field_Index    : constant One_Bit_Per_Interrupt_Field_Index_Type :=
        One_Bit_Per_Interrupt_Field_Index_Type (External_Interrupt_Id mod
                                                GIC_ICENABLER_Type'Length);
   begin
      --
      --  NOTE: We need to serialize access to the GICD from multiple
      --  CPU cores, as external interrupts (shared peripheral interrupts)
      --  are not per CPU core.
      --
      CPU.Multicore.Spinlock_Acquire (Interrupt_Controller_Obj.Spinlock);
      GIC_ICENABLER_Value                                  := [others => 0];
      GIC_ICENABLER_Value (ICENABLER_Field_Index)          := 1;
      GICD.GICD_ICENABLER_Array (ICENABLER_Register_Index) := GIC_ICENABLER_Value;
      CPU.Multicore.Spinlock_Release (Interrupt_Controller_Obj.Spinlock);
   end Disable_External_Interrupt;

   procedure GIC_Interrupt_Handler
     (Cpu_Interrupt_Line : Cpu_Interrupt_Line_Type)
   is
      use type Interfaces.Unsigned_16;
      ICC_IAR_Value : constant ICC_IAR_Type := GICC.ICC_IAR;
      Interrupt_Id : constant Interfaces.Unsigned_16 := Interfaces.Unsigned_16 (ICC_IAR_Value.INTID);
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type  := CPU.Multicore.Get_Cpu_Id;
      Old_Cpu_Interrupting_State : CPU.Cpu_Register_Type with Unreferenced;
      ICC_EOIR_Value : ICC_EOIR_Type;
   begin
      --  Enable interrupts at the CPU to support nested interrupts
      CPU.Interrupt_Handling.Enable_Cpu_Interrupting;

      --  Invoke the IRQ-specific interrupt handler:
      if Interrupt_Id <= Interfaces.Unsigned_16 (Internal_Interrupt_Id_Type'Last) then
         declare
            Internal_Interrupt_Id : constant Internal_Interrupt_Id_Type := Internal_Interrupt_Id_Type (Interrupt_Id);
            Interrupt_Handler : Interrupt_Handler_Type renames
               Interrupt_Controller_Obj.Internal_Interrupt_Handlers (Cpu_Id, Internal_Interrupt_Id);
         begin
            if Interrupt_Handler.Interrupt_Handler_Entry_Point /= null then
               Interrupt_Handler.Interrupt_Handler_Entry_Point (Interrupt_Handler.Interrupt_Handler_Arg);
            end if;
         end;
      elsif Interrupt_Id <= Interfaces.Unsigned_16 (External_Interrupt_Id_Type'Last) then
         declare
            External_Interrupt_Id : constant External_Interrupt_Id_Type := External_Interrupt_Id_Type (Interrupt_Id);
            Interrupt_Handler : Interrupt_Handler_Type renames
               Interrupt_Controller_Obj.External_Interrupt_Handlers (External_Interrupt_Id);
         begin
            pragma Assert (Interrupt_Handler.Interrupt_Handler_Entry_Point /= null);
            Interrupt_Handler.Interrupt_Handler_Entry_Point (Interrupt_Handler.Interrupt_Handler_Arg);
         end;
      else
         Utils.Print_String ("*** Special ");
         Utils.Print_String (
            (if Cpu_Interrupt_Line = Cpu_Interrupt_Fiq then "FIQ" else "IRQ"));
         Utils.Print_String (" interrupt ");
         Utils.Print_Number_Decimal (Interfaces.Unsigned_32 (Interrupt_Id), End_Line => True);
         --
         --  NOTE: These INTIDs do not require an end of interrupt or deactivation.
         --
         return;
      end if;

      --  Disable interrupts at the CPU before returning:
      Old_Cpu_Interrupting_State := CPU.Interrupt_Handling.Disable_Cpu_Interrupting;

      --
      --  Notify the interrupt controller that processing for the last interrupt
      --  received by the calling CPU core has been completed, so that another
      --  interrupt of the same priority or lower can be received by this CPU core:
      --
      CPU.Strong_Memory_Barrier;
      ICC_EOIR_Value.INTID := INTID_Type (Interrupt_Id);
      GICC.ICC_EOIR := ICC_EOIR_Value;
   end GIC_Interrupt_Handler;

   procedure Trigger_Software_Generated_Interrupt (Soft_Gen_Interrupt_Id : Soft_Gen_Interrupt_Id_Type;
                                                   Cpu_Id : CPU.Valid_Cpu_Core_Id_Type) is
      GICD_SGIR_Value : GICD_SGIR_Type;
   begin
      GICD_SGIR_Value.Target_List :=
         GICD_SGIR_Target_List_Type (Bit_Mask (Bit_Index_Type (Cpu_Id)));
      GICD_SGIR_Value.INTID := SGI_INTID_Type (Soft_Gen_Interrupt_Id);
      GICD.GICD_SGIR := GICD_SGIR_Value;
   end Trigger_Software_Generated_Interrupt;

   function Get_Highest_Interrupt_Priority_Disabled return Interrupt_Priority_Type is
      ICC_PMR_Value : constant ICC_PMR_Type := GICC.ICC_PMR;
   begin
      return Interrupt_Priority_Type (ICC_PMR_Value.Priority);
   end Get_Highest_Interrupt_Priority_Disabled;

   procedure Set_Highest_Interrupt_Priority_Disabled (Priority : Interrupt_Priority_Type) is
      ICC_PMR_Value : ICC_PMR_Type;
   begin
      ICC_PMR_Value.Priority := GIC_Interrupt_Priority_Type (Priority);
      GICC.ICC_PMR := ICC_PMR_Value;
   end Set_Highest_Interrupt_Priority_Disabled;

end Interrupt_Controller_Driver;
