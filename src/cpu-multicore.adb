--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Multicore Utilities
--

with Board;
with Utils.Runtime_Log;
with CPU.Interrupt_Handling;
with System.Machine_Code;
with Interfaces;

package body CPU.Multicore is

   function Get_Cpu_Id return Valid_Cpu_Core_Id_Type is
      MPIDR_EL1_Value : MPIDR_EL1_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, mpidr_el1",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", MPIDR_EL1_Value.Value), --  %0
         Volatile => True);

      return Valid_Cpu_Core_Id_Type (
         (if MPIDR_EL1_Value.MT = 1 then MPIDR_EL1_Value.Aff1
                                    else MPIDR_EL1_Value.Aff0));
   end Get_Cpu_Id;

   procedure Wait_For_Multicore_Event is
   begin
      System.Machine_Code.Asm ("wfe", Volatile => True);
   end Wait_For_Multicore_Event;

   procedure Send_Multicore_Event is
   begin
      System.Machine_Code.Asm ("sev", Volatile => True);
   end Send_Multicore_Event;

   procedure Start_Secondary_Cpus is
      use Utils.Runtime_Log;
      Reset_Handler_Address : constant System.Address := Get_Reset_Handler_Address;
   begin
      for Cpu_Id in Secondary_Cpu_Core_Id_Type loop
         Board.Start_Secondary_Cpu (Cpu_Id, Reset_Handler_Address);
         Log_Info_Msg_Begin ("CPU");
         Log_Info_Value_Decimal (Interfaces.Unsigned_32 (Cpu_Id));
         Log_Info_Msg_End (" started");
      end loop;
   end Start_Secondary_Cpus;

   function Ldaex (Address : System.Address) return Cpu_Register_Type is
      Result : Cpu_Register_Type;
   begin
      System.Machine_Code.Asm (
           "ldaxr %0, [%1]",
           Outputs => Cpu_Register_Type'Asm_Output ("=r", Result), --  %0
           Inputs => System.Address'Asm_Input ("r", Address), --  %1
           Volatile => True);

      return Result;
   end Ldaex;

   function Stlex (Address : System.Address;
                   Value : Cpu_Register_Type) return Boolean
   is
      Result : Cpu_Register_Type;
   begin
      System.Machine_Code.Asm (
           "stlxr w0, %1, [%2]" & ASCII.LF &
           "mov %0, x0",
           Outputs =>
              --  NOTE: Use "=&r" to ensure a different register is used
              Cpu_Register_Type'Asm_Output ("=&r", Result),   -- %0
           Inputs =>
              [Cpu_Register_Type'Asm_Input ("r", Value),      -- %1
               System.Address'Asm_Input ("r", Address)], -- %2
           Clobber => "x0, memory",
           Volatile => True);

      return Result = 0;
   end Stlex;

   type Atomic_Operator_Type is (Fetch_Add,
                                 Fetch_Sub,
                                 Fetch_Or,
                                 Fetch_And);

   function Atomic_Operation (Atomic_Operator : Atomic_Operator_Type;
                              Atomic_Counter : in out Atomic_Counter_Type;
                              Value : Cpu_Register_Type) return Cpu_Register_Type
    with Inline_Always,
         Pre => Cpu_Is_Multicore_Synchronization_Ready
   is
      Old_Value : Cpu_Register_Type;
      New_Value : Cpu_Register_Type;
   begin
      loop
         Old_Value := Ldaex (Atomic_Counter.Counter'Address);
         case Atomic_Operator is
            when Fetch_Add =>
               New_Value := Old_Value + Value;
            when Fetch_Sub =>
               New_Value := Old_Value - Value;
            when Fetch_Or =>
               New_Value := Old_Value or Value;
            when Fetch_And =>
               New_Value := Old_Value and Value;
         end case;

         exit when Stlex (Atomic_Counter.Counter'Address, New_Value);
      end loop;

      return Old_Value;
   end Atomic_Operation;

   procedure Atomic_Counter_Initialize (Atomic_Counter_Obj : out Atomic_Counter_Type;
                                        Value : Cpu_Register_Type) is
   begin
      Atomic_Counter_Obj.Counter := Value;
   end Atomic_Counter_Initialize;

   function Atomic_Fetch_Add (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type is
      (Atomic_Operation (Fetch_Add, Atomic_Counter, Value));

   function Atomic_Fetch_Sub (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type is
      (Atomic_Operation (Fetch_Sub, Atomic_Counter, Value));

   function Atomic_Fetch_Or (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type is
      (Atomic_Operation (Fetch_Or, Atomic_Counter, Value));

   function Atomic_Fetch_And (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type is
      (Atomic_Operation (Fetch_And, Atomic_Counter, Value));

   function Atomic_Load (Atomic_Counter : Atomic_Counter_Type)
    return Cpu_Register_Type
   is
   begin
      return Atomic_Counter.Counter;
   end Atomic_Load;

   procedure Atomic_Store (Atomic_Counter : out Atomic_Counter_Type; Value : Cpu_Register_Type)
   is
   begin
      Atomic_Counter.Counter := Value;
   end Atomic_Store;

   procedure Spinlock_Acquire (Spinlock : in out Spinlock_Type) is
      Cpu_Id : constant Valid_Cpu_Core_Id_Type := Get_Cpu_Id;
      Old_Cpu_Interrupting : constant Cpu_Register_Type :=
         CPU.Interrupt_Handling.Disable_Cpu_Interrupting;
   begin
      if Spinlock_Owner (Spinlock) = Cpu_Id then
         Spinlock.Recursive_Acquire_Count := @ + 1;
         CPU.Interrupt_Handling.Restore_Cpu_Interrupting (Old_Cpu_Interrupting);
         return;
      end if;

      declare
         My_Ticket : constant Cpu_Register_Type := Atomic_Fetch_Add (Spinlock.Next_Ticket, 1);
      begin
         while Spinlock.Now_Serving /= My_Ticket loop
            Wait_For_Multicore_Event;
         end loop;

         pragma Assert (Spinlock.Owner = Invalid_Cpu_Core_Id);
         Spinlock.Owner := Cpu_Id;
         Spinlock.Old_Cpu_Interrupting := Old_Cpu_Interrupting;
         Memory_Barrier;
      end;
   end Spinlock_Acquire;

   procedure Spinlock_Release (Spinlock : in out Spinlock_Type) is
   begin
      if Spinlock.Recursive_Acquire_Count > 0 then
         Spinlock.Recursive_Acquire_Count := @ - 1;
         return;
      end if;

      Memory_Barrier;
      Spinlock.Owner := Invalid_Cpu_Core_Id;
      Spinlock.Now_Serving := @ + 1;
      Send_Multicore_Event;
      CPU.Interrupt_Handling.Restore_Cpu_Interrupting (Spinlock.Old_Cpu_Interrupting);
   end Spinlock_Release;

end CPU.Multicore;