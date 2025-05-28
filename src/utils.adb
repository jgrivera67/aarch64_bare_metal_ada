--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with CPU.Multicore;
with Timer_Driver;
with Utils.Number_Conversion;

package body Utils is
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

   procedure Copy_String (Dest : out String;
                          Source : String;
                          Cursor_Index : in out Positive) is
   begin
      Dest (Cursor_Index .. Cursor_Index + Source'Length - 1) := Source;
      Cursor_Index := @ + Source'Length;
   end Copy_String;

   Console_Spinlock : CPU.Multicore.Spinlock_Type;

   procedure Lock_Console (Print_Cpu : Boolean) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
   begin
      if not CPU.Mmu_Is_Enabled then
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
      if not CPU.Mmu_Is_Enabled then
         return;
      end if;

      CPU.Multicore.Spinlock_Release (Console_Spinlock);
   end Unlock_Console;

   function Receive_Byte_With_Timeout (Timeout_Usec : Interfaces.Unsigned_64)
      return Uart_Driver.Maybe_Byte_Type
   is
      use type Interfaces.Unsigned_64;
      Maybe_Byte : Uart_Driver.Maybe_Byte_Type;
      Start_Timestamp_Usec : constant Interfaces.Unsigned_64 := Timer_Driver.Get_Timestamp_Usec;
   begin
      loop
         Maybe_Byte := Uart_Driver.Receive_Byte_If_Any;
         exit when Maybe_Byte.Valid or else
                   Timer_Driver.Get_Timestamp_Usec - Start_Timestamp_Usec >= Timeout_Usec;
      end loop;

      return Maybe_Byte;
   end Receive_Byte_With_Timeout;

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
      Lock_Console (Print_Cpu => False);
      Print_String (ASCII.LF & "*** CPU");
      Print_Number_Decimal (Interfaces.Unsigned_32 (Cpu_Id));
      if Last_Chance_Handler_Running (Cpu_Id) then
         Print_String (" Recursive");
      end if;

      Print_String (" Exception: '");
      Print_String (Msg_Text (1 .. Msg_Length));
      if Line /= 0 then
         Print_String ("' at line ");
         Print_Number_Decimal (Interfaces.Unsigned_32 (Line), End_Line => True);
      else
         Print_String ("'" & ASCII.LF);
      end if;
      Unlock_Console;

      if not Last_Chance_Handler_Running (Cpu_Id) then
         Last_Chance_Handler_Running (Cpu_Id) := True;
         --  Break into the self-hosted debugger:
         CPU.Break_Point;
      end if;

      CPU.Park_Cpu;
   end Last_Chance_Handler;
end Utils;