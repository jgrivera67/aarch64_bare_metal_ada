--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with CPU.Interrupt_Handling;
with Utils.Number_Conversion;
with Utils.Runtime_Log;

package body Utils is
   use Utils.Runtime_Log;

   Last_Chance_Handler_Running : array (CPU.Valid_Cpu_Core_Id_Type) of Boolean :=
      [others => False];

   procedure Put_Char (C : Character) is
   begin
      Uart_Driver.Put_Char (C);
      if C = ASCII.LF then
         Uart_Driver.Put_Char (ASCII.CR);
      end if;
   end Put_Char;

   procedure Print_String (S : String; End_Line : Boolean := False) is
   begin
      for C of S loop
         Put_Char (C);
      end loop;

      if End_Line then
         Put_Char (ASCII.LF);
      end if;
   end Print_String;

   procedure Print_Number_Decimal (Value : Interfaces.Unsigned_32;
                                   End_Line : Boolean := False)
   is
      Str : String (1 .. 10);
      Str_Len : Positive;
   begin
      Utils.Number_Conversion.Unsigned_To_Decimal_String (Value, Str, Str_Len);
      Print_String (Str (1 .. Str_Len), End_Line);
   end Print_Number_Decimal;

   procedure Print_Number_Hexadecimal (Value : Interfaces.Unsigned_64;
                                       End_Line : Boolean := False)
   is
      Str : String (1 .. 18);
   begin
      Str (1 .. 2) := "0x";
      Utils.Number_Conversion.Unsigned_To_Hexadecimal_String (Value, Str (3 .. 18));
      Print_String (Str, End_Line);
   end Print_Number_Hexadecimal;

   procedure Print_Number_Hexadecimal (Value : Interfaces.Unsigned_8;
                                       End_Line : Boolean := False)
   is
      Str : String (1 .. 4);
   begin
      Str (1 .. 2) := "0x";
      Utils.Number_Conversion.Unsigned_To_Hexadecimal_String (Value, Str (3 .. 4));
      Print_String (Str, End_Line);
   end Print_Number_Hexadecimal;

   procedure Copy_String (Dest : out String;
                          Source : String;
                          Cursor_Index : in out Positive) is
   begin
      Dest (Cursor_Index .. Cursor_Index + Source'Length - 1) := Source;
      Cursor_Index := @ + Source'Length;
   end Copy_String;

   function Equal_Strings (Str1, Str2 : String) return Boolean is
      use System.Storage_Elements;
      Machine_Alignment : constant := Integer_Address'Size / System.Storage_Unit;
   begin
      if Str1'Length /= Str2'Length then
         return False;
      end if;

      --
      --  NOTE: Comparing strings as arrays can cause data aborts, as the compiler
      --  generates multi-byte load instructions, even when the strings are not properly
      --  aligned. The data abort happens even with alignment check exceptions disabled
      --  in the AArch64 core (at least on Raspberry PI).
      --
      if To_Integer (Str1'Address) mod Machine_Alignment = 0 and then
         To_Integer (Str2'Address) mod Machine_Alignment = 0
      then
         --  If both strings are aligned, we can compare them as arrays
         return Str1 = Str2;
      end if;

      for I in 0 .. Str1'Length - 1 loop
         if Str1 (Str1'First + I) /= Str2 (Str2'First + I) then
            return False;
         end if;
      end loop;

      return True;
   end Equal_Strings;

   procedure Lock_Console (Print_Cpu : Boolean := True) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
   begin
      if not CPU.Cpu_Is_Multicore_Synchronization_Ready then
         return;
      end if;

      CPU.Multicore.Spinlock_Acquire (Console_Spinlock);
      if Print_Cpu then
         Print_String ("CPU");
         Print_Number_Decimal (Interfaces.Unsigned_32 (Cpu_Id));
         Print_String (": ");
      end if;
   end Lock_Console;

   procedure Unlock_Console is
   begin
      if not CPU.Cpu_Is_Multicore_Synchronization_Ready then
         return;
      end if;

      Uart_Driver.Flush_Output;
      CPU.Multicore.Spinlock_Release (Console_Spinlock);
   end Unlock_Console;

   function Receive_Byte_With_Timeout (Timeout_Us : Timer_Driver.Delta_Time_Us_Type)
      return Uart_Driver.Maybe_Byte_Type
   is
      use Timer_Driver;
      Maybe_Byte : Uart_Driver.Maybe_Byte_Type;
      Start_Timestamp_Us : constant Timestamp_Us_Type := Get_Timestamp_Us;
      Delta_Time_Us : Delta_Time_Us_Type;
   begin
      loop
         Maybe_Byte := Uart_Driver.Receive_Byte_If_Any;
         Delta_Time_Us := Delta_Time_Us_Type (Get_Timestamp_Us - Start_Timestamp_Us);
         exit when Maybe_Byte.Valid or else Delta_Time_Us >= Timeout_Us;
      end loop;

      return Maybe_Byte;
   end Receive_Byte_With_Timeout;

   procedure Wait_For_Ctrl_C is
   begin
      Lock_Console;
      Print_String ("Press CTRL-C to continue" & ASCII.LF);
      Unlock_Console;

      --
      --   Wait for CTRL-C to be received on the UART
      --
      if Uart_Driver.Input_Interrupt_Enabled then
         declare
            Byte : Uart_Driver.Maybe_Byte_Type;
         begin
            loop
               Byte := Uart_Driver.Receive_Byte_If_Any;
               exit when Byte.Valid and then
                         Character'Val (Byte.Byte) = Ctrl_C;

               CPU.Interrupt_Handling.Wait_For_Interrupt;
            end loop;
         end;
      else
         declare
            C : Character;
         begin
            loop
               C := Get_Char;
               exit when C = Ctrl_C;
            end loop;
         end;
      end if;

      Lock_Console (Print_Cpu => False);
      Print_String (ASCII.LF & "^C" & ASCII.LF);
   end Wait_For_Ctrl_C;

   function Get_PC_Here return System.Address is
      Call_Address : constant System.Address := CPU.Get_Call_Address;
   begin
      return Call_Address;
   end Get_PC_Here;

   procedure System_Crash is
      Old_Cpu_Interrupting_State : CPU.Cpu_Register_Type with Unreferenced;
   begin
      Old_Cpu_Interrupting_State := CPU.Interrupt_Handling.Disable_Cpu_Interrupting;
      Log_Info_Msg ("*** System Crashing ***");
      Wait_For_Ctrl_C;

      Lock_Console;
      Print_String ("*** System crashed ***" & ASCII.LF);
      Dump_Runtime_Log;
      Unlock_Console;

      Log_Info_Msg ("*** Parking CPU ***" & ASCII.LF);
      CPU.Park_Cpu;
   end System_Crash;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Msg_Text : String (1 .. 128) with Address => Msg;
      Msg_Length : Natural := 0;
   begin
      --
      --  Calculate length of the null-terminated 'Msg' string:
      --
      for Msg_Char of Msg_Text loop
         exit when Msg_Char = ASCII.NUL;
         Msg_Length := Msg_Length + 1;
      end loop;

      --
      --  Print exception message to UART:
      --
      if Last_Chance_Handler_Running (Cpu_Id) then
         Log_Error_Msg_Begin ("*** Recursive Ada Exception: '");
      else
         Log_Error_Msg_Begin ("*** Ada Exception: '");
      end if;

      Log_Error_Msg_Part (Msg_Text (1 .. Msg_Length));
      if Line /= 0 then
         Log_Error_Msg_Part ("' at line ");
         Log_Error_Value_Decimal (Interfaces.Unsigned_32 (Line));
      else
         Log_Error_Msg_Part ("'");
      end if;

      Log_Error_Msg_End (" ***" & ASCII.LF);
      if not Last_Chance_Handler_Running (Cpu_Id) then
         Last_Chance_Handler_Running (Cpu_Id) := True;
         --  Break into the self-hosted debugger:
         CPU.Break_Point;
      end if;

      System_Crash;
   end Last_Chance_Handler;
end Utils;