--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Multicore Utilities
--

package CPU.Multicore
   with SPARK_Mode => On
is
   Debug_On : Boolean := False;

   function Get_Cpu_Id return Valid_Cpu_Core_Id_Type
      with Inline_Always,
           Suppress => All_Checks;

   procedure Wait_For_Multicore_Event with
      Inline_Always;

   procedure Send_Multicore_Event with
      Inline_Always;

   procedure Start_Secondary_Cpus
      with Pre => Get_Cpu_Id = Valid_Cpu_Core_Id_Type'First;

   type Atomic_Counter_Type is limited private;

   function Atomic_Counter_Initializer (Value : Cpu_Register_Type) return Atomic_Counter_Type;

   procedure Atomic_Counter_Initialize (Atomic_Counter_Obj : out Atomic_Counter_Type;
                                        Value : Cpu_Register_Type);

   function Atomic_Fetch_Add (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type
    with SPARK_Mode => Off;

   function Atomic_Fetch_Sub (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type
    with SPARK_Mode => Off;

   function Atomic_Fetch_Or (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type
    with SPARK_Mode => Off;

   function Atomic_Fetch_And (Atomic_Counter : in out Atomic_Counter_Type; Value : Cpu_Register_Type)
    return Cpu_Register_Type
    with SPARK_Mode => Off;

   function Atomic_Load (Atomic_Counter : Atomic_Counter_Type)
    return Cpu_Register_Type;

   procedure Atomic_Store (Atomic_Counter : out Atomic_Counter_Type; Value : Cpu_Register_Type);

   type Spinlock_Type is limited private;

   function Spinlock_Owner (Spinlock : Spinlock_Type) return Cpu_Core_Id_Type;

   procedure Spinlock_Acquire (Spinlock : in out Spinlock_Type)
      with Pre => Cpu_In_Privileged_Mode and then
                  Cpu_Is_Multicore_Synchronization_Ready,
           Post => Spinlock_Owner (Spinlock) = Get_Cpu_Id and then
                   Cpu_Interrupting_Disabled;

   procedure Spinlock_Release (Spinlock : in out Spinlock_Type)
      with Pre => Cpu_In_Privileged_Mode and then
                  Spinlock_Owner (Spinlock) = Get_Cpu_Id and then
                  Cpu_Interrupting_Disabled;

private

   type Atomic_Counter_Type is limited record
      Counter : Cpu_Register_Type := 0 with Volatile_Full_Access;
   end record
     with Size => Cache_Line_Size_In_Bytes * System.Storage_Unit,
          Alignment => Cache_Line_Size_In_Bytes;

   function Atomic_Counter_Initializer (Value : Cpu_Register_Type) return Atomic_Counter_Type is
      ((Counter => Value));

   type Recursive_Acquire_Count_Type is range 0 .. 32;

   --
   --  Fair spinlock object
   --
   type Spinlock_Type is limited record
      --  Ticket number to be assigned to the next caller of spinlock_acquire()
      Next_Ticket : Atomic_Counter_Type;
      --  Ticket number assigned to the current owner of the spinlock
      Now_Serving : Cpu_Register_Type := 0 with Atomic;
      --  CPU interrupt mask before interrupts were disabled when acquiring the spinlock.
      Old_Cpu_Interrupting : Cpu_Register_Type := 0;
      --  Counter of recursive/nested Spinlock_Acquire() callsi by the owning CPU
      Recursive_Acquire_Count : Recursive_Acquire_Count_Type := 0;
      --  Inter-cluster CPU core ID
      Owner : Cpu_Core_Id_Type := Invalid_Cpu_Core_Id;
   end record with Alignment => Cache_Line_Size_In_Bytes;

   function Spinlock_Owner (Spinlock : Spinlock_Type) return Cpu_Core_Id_Type is
      (Spinlock.Owner);
end CPU.Multicore;
