--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Uart_Driver;
with Uart_Boot_Loader.Xmodem_Protocol;
with GNAT.Source_Info;
with System.Machine_Code;

package body Uart_Boot_Loader is
   Last_Chance_Handler_Running : Boolean := False;

   procedure Load_Image_Over_Uart (Load_Address : System.Address) is
   begin
      Print_String (
        "Raspberry PI 4 UART boot loader (built on " &
        GNAT.Source_Info.Compilation_Date &
        " at " & GNAT.Source_Info.Compilation_Time & ")" & ASCII.LF);

      Xmodem_Protocol.Receive_File (Load_Address);
   end Load_Image_Over_Uart;

   procedure Jump_To_Image_Reset_Handler (Reset_Handler_Address : System.Address) is
   begin
      System.Machine_Code.Asm (
          "dmb sy" & ASCII.LF &
          "br %0",
           Inputs => System.Address'Asm_Input ("r", Reset_Handler_Address),  --  %0
           Volatile => True);
   end Jump_To_Image_Reset_Handler;

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
         Msg_Length := Msg_Length + 1;
         exit when Msg_Char = ASCII.NUL;
      end loop;

      if Last_Chance_Handler_Running then
         Print_String (
            "*** Recursive call to Last_Chance_Handler: " &
            Msg_Text (1 .. Msg_Length) & "' at line ");
         Print_Number_Decimal (Interfaces.Unsigned_32 (Line), End_Line => True);
         loop
            null;
         end loop;
      end if;

      Last_Chance_Handler_Running := True;

      --
      --  Print exception message to UART:
      --
      if Line /= 0 then
         Print_String (
            ASCII.LF & "*** Exception: '" & Msg_Text (1 .. Msg_Length) &
            "' at line ");
         Print_Number_Decimal (Interfaces.Unsigned_32 (Line), End_Line => True);
      else
         Print_String (
            ASCII.LF &
            "*** Exception: '" & Msg_Text (1 .. Msg_Length) & "'" & ASCII.LF);
      end if;

      loop
         null;
      end loop;
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

end Uart_Boot_Loader;
