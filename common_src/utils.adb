--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Machine_Code;
with System.Storage_Elements;

package body Utils is
   Last_Chance_Handler_Running : Boolean := False;

   procedure Print_String (S : String; End_Line : Boolean := False) is
   begin
      for C of S loop
         Uart_Driver.Put_Char (C);
         if C = ASCII.LF then
            Uart_Driver.Put_Char (ASCII.CR);
         end if;
      end loop;

      if End_Line then
         Uart_Driver.Put_Char (ASCII.LF);
         Uart_Driver.Put_Char (ASCII.CR);
      end if;
   end Print_String;

   procedure Print_Number_Decimal (Value : Interfaces.Unsigned_32;
                                   End_Line : Boolean := False)
   is
      procedure Unsigned_To_Decimal_String (Value : Interfaces.Unsigned_32;
                                            Buffer : out String;
                                            Actual_Length : out Positive;
                                            Add_Leading_Zeros : Boolean := False)
      is
         use type Interfaces.Unsigned_32;
         Tmp_Buffer : String (1 .. 10);
         Start_Index : Positive range Tmp_Buffer'Range := Tmp_Buffer'First;
         Value_Left : Interfaces.Unsigned_32 := Value;
      begin
         for I in reverse Tmp_Buffer'Range loop
            Tmp_Buffer (I) := Character'Val ((Value_Left mod 10) +
                                             Character'Pos ('0'));
            Value_Left := Value_Left / 10;
            if Value_Left = 0 then
               Start_Index := I;
               exit;
            end if;
         end loop;

         Actual_Length := (Tmp_Buffer'Last - Start_Index) + 1;
         if Buffer'Length >= Actual_Length then
            if Add_Leading_Zeros then
               Buffer (Buffer'First .. Buffer'Last - Actual_Length) :=
                  [others => '0'];
               Buffer (Buffer'Last - Actual_Length + 1 .. Buffer'Last) :=
                  Tmp_Buffer (Start_Index .. Tmp_Buffer'Last);
               Actual_Length := Buffer'Length;
            else
               Buffer (Buffer'First .. Buffer'First + Actual_Length - 1) :=
                  Tmp_Buffer (Start_Index .. Tmp_Buffer'Last);
            end if;
         else
            raise Program_Error
               with "Unsigned_To_Decimal: buffer too small";
         end if;
      end Unsigned_To_Decimal_String;

      Str : String (1 .. 10);
      Str_Len : Positive;
   begin
      Unsigned_To_Decimal_String (Value, Str, Str_Len);
      Print_String (Str (1 .. Str_Len), End_Line);
   end Print_Number_Decimal;

   procedure Print_Number_Hexadecimal (Value : Interfaces.Unsigned_64;
                                       End_Line : Boolean := False)
   is
      procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_64;
                                                Buffer : out String)
      is
         use type Interfaces.Unsigned_8;
         Hex_Digit : Interfaces.Unsigned_8 range 16#0# .. 16#f#;
         Value_Left : Interfaces.Unsigned_64 := Value;
      begin
         for I in reverse Buffer'Range loop
            Hex_Digit := Interfaces.Unsigned_8 (Value_Left and 16#f#);
            if Hex_Digit < 16#a# then
               Buffer (I) := Character'Val (Hex_Digit + Character'Pos ('0'));
            else
               Buffer (I) := Character'Val ((Hex_Digit - 16#a#) +
                                             Character'Pos ('A'));
            end if;

            Value_Left := Interfaces.Shift_Right (Value_Left, 4);
         end loop;

         pragma Assert (Value_Left = 0);
      end Unsigned_To_Hexadecimal_String;

      Str : String (1 .. 18);
   begin
      Str (1 .. 2) := "0x";
      Unsigned_To_Hexadecimal_String (Value, Str (3 .. 18));
      Print_String (Str, End_Line);
   end Print_Number_Hexadecimal;

   function Compute_Checksum (Bytes_Array : Byte_Array_Type)
      return Interfaces.Unsigned_32
   is
      use type Interfaces.Unsigned_32;
      CRC_32_Polynomial : constant := 16#04c11db7#;
      CRC : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;
      Data_Byte : Interfaces.Unsigned_8;
   begin
      for B of Bytes_Array loop
         Data_Byte := B;
         for I in 1 .. 8 loop
            if ((Interfaces.Unsigned_32 (Data_Byte) xor CRC) and 1) /= 0 then
               CRC := Interfaces.Shift_Right (CRC, 1);
               Data_Byte := Interfaces.Shift_Right (Data_Byte, 1);
               CRC := CRC xor CRC_32_Polynomial;
            else
               CRC := Interfaces.Shift_Right (CRC, 1);
               Data_Byte := Interfaces.Shift_Right (Data_Byte, 1);
            end if;
         end loop;
      end loop;

      return CRC;
   end Compute_Checksum;

   function Receive_Byte_With_Timeout (Timeout_Usec : Interfaces.Unsigned_64)
      return Uart_Driver.Maybe_Byte_Type
   is
      Maybe_Byte : Uart_Driver.Maybe_Byte_Type;
      Start_Timestamp_Usec : constant Interfaces.Unsigned_64 := Get_Timer_Timestamp_Usec;
   begin
      loop
         Maybe_Byte := Uart_Driver.Receive_Byte_If_Any;
         exit when Maybe_Byte.Valid or else
                   Get_Timer_Timestamp_Usec - Start_Timestamp_Usec >= Timeout_Usec;
      end loop;

      return Maybe_Byte;
   end Receive_Byte_With_Timeout;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
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

      if Last_Chance_Handler_Running then
         Print_String ("*** Recursive call to Last_Chance_Handler: '");
         Print_String (Msg_Text (1 .. Msg_Length));
         Print_String ("'" & ASCII.LF);
         Park_Cpu;
      end if;

      Last_Chance_Handler_Running := True;

      --
      --  Print exception message to UART:
      --
      Print_String (ASCII.LF & "*** Exception: '");
      Print_String (Msg_Text (1 .. Msg_Length));
      if Line /= 0 then
         Print_String ("' at line ");
         Print_Number_Decimal (Interfaces.Unsigned_32 (Line), End_Line => True);
      else
         Print_String ("'" & ASCII.LF);
      end if;

      --
      --  Break into the debugger:
      --
      Break_Point;

      Park_Cpu;
   end Last_Chance_Handler;

   function Get_CNTPCT return Interfaces.Unsigned_64 is
      CNTPCT_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntpct_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTPCT_Value), --  %0
         Volatile => True);

      return CNTPCT_Value;
   end Get_CNTPCT;

   function Get_CNTFRQ return Interfaces.Unsigned_64 is
      CNTFRQ_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntfrq_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTFRQ_Value), --  %0
         Volatile => True);

      return CNTFRQ_Value;
   end Get_CNTFRQ;

   function Get_Call_Address return System.Address is
      Reg_Value : Interfaces.Unsigned_64;
      Call_Instruction_Size_In_Bytes : constant := 4;
   begin
      System.Machine_Code.Asm (
         "mov %0, lr",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", Reg_Value),
         Volatile => True);

      return System.Storage_Elements.To_Address (
               System.Storage_Elements.Integer_Address (
                  Reg_Value - Call_Instruction_Size_In_Bytes));
   end Get_Call_Address;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm (
         "wfi",
         Volatile => True);
   end Wait_For_Interrupt;

   procedure Break_Point is
   begin
      System.Machine_Code.Asm ("brk #0", Volatile => True);
   end Break_Point;

end Utils;
