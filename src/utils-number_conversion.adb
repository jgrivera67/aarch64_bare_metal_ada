--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Utils.Number_Conversion is
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

   -----------------------------------------------------------------------------
   --  Generic numeric conversion utilities
   -----------------------------------------------------------------------------
   package body Generic_Number_Conversion_Utils is
      procedure Unsigned_To_Hexadecimal_String (Value : Unsigned_Type;
                                                Buffer : out String)
      is
         Hex_Digit : Unsigned_Type range 16#0# .. 16#f#;
         Value_Left : Unsigned_Type := Value;
      begin
         for I in reverse Buffer'Range loop
            Hex_Digit := Value_Left and 16#f#;
            if Hex_Digit < 16#a# then
               Buffer (I) := Character'Val (Hex_Digit + Character'Pos ('0'));
            else
               Buffer (I) := Character'Val ((Hex_Digit - 16#a#) +
                                             Character'Pos ('A'));
            end if;

            Value_Left := Shift_Right_Func (Value_Left, 4);
         end loop;

         pragma Assert (Value_Left = 0);
      end Unsigned_To_Hexadecimal_String;

      procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                                Value : out Unsigned_Type;
                                                Conversion_Ok : out Boolean)
      is
         Prev_Value : Unsigned_Type;
         Hexadecimal_Digit : Unsigned_Type;
      begin
         Value := 0;
         for C of Hexadecimal_Str loop
            if C in '0' .. '9' then
               Hexadecimal_Digit := Character'Pos (C) - Character'Pos ('0');
            elsif C in 'A' .. 'F' then
               Hexadecimal_Digit := Character'Pos (C) - Character'Pos ('A') + 10;
            elsif C in 'a' .. 'f' then
               Hexadecimal_Digit := Character'Pos (C) - Character'Pos ('a') + 10;
            else
               Conversion_Ok := False;
               return;
            end if;

            Prev_Value := Value;
            Value := Shift_Left_Func (Value, 4) or Hexadecimal_Digit;
            if Value < Prev_Value then
               --  Number is too big
               Conversion_Ok := False;
               return;
            end if;
         end loop;

         Conversion_Ok := True;
      end Hexadecimal_String_To_Unsigned;
   end Generic_Number_Conversion_Utils;

   package Unsigned_64_Number_Conversion_Utils
     is new Generic_Number_Conversion_Utils
       (Unsigned_Type => Interfaces.Unsigned_64,
        Shift_Right_Func => Interfaces.Shift_Right,
        Shift_Left_Func => Interfaces.Shift_Left);

   package Unsigned_32_Number_Conversion_Utils
     is new Generic_Number_Conversion_Utils
       (Unsigned_Type => Interfaces.Unsigned_32,
        Shift_Right_Func => Interfaces.Shift_Right,
        Shift_Left_Func => Interfaces.Shift_Left);

   package Unsigned_16_Number_Conversion_Utils
     is new Generic_Number_Conversion_Utils
       (Unsigned_Type => Interfaces.Unsigned_16,
        Shift_Right_Func => Interfaces.Shift_Right,
        Shift_Left_Func => Interfaces.Shift_Left);

   package Unsigned_8_Number_Conversion_Utils
     is new Generic_Number_Conversion_Utils
       (Unsigned_Type => Interfaces.Unsigned_8,
        Shift_Right_Func => Interfaces.Shift_Right,
        Shift_Left_Func => Interfaces.Shift_Left);

   function Shift_Right (Value : System.Storage_Elements.Integer_Address; Shift : Natural)
     return System.Storage_Elements.Integer_Address is
     (System.Storage_Elements.Integer_Address (
         Interfaces.Shift_Right (Interfaces.Unsigned_64 (Value), Shift)));

   function Shift_Left (Value : System.Storage_Elements.Integer_Address; Shift : Natural)
     return System.Storage_Elements.Integer_Address is
     (System.Storage_Elements.Integer_Address (
         Interfaces.Shift_Left (Interfaces.Unsigned_64 (Value), Shift)));

   package Integer_Address_Number_Conversion_Utils
     is new Generic_Number_Conversion_Utils
       (Unsigned_Type => System.Storage_Elements.Integer_Address,
        Shift_Right_Func => Shift_Right,
        Shift_Left_Func => Shift_Left);

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_64;
                                             Conversion_Ok : out Boolean)
   renames Unsigned_64_Number_Conversion_Utils.Hexadecimal_String_To_Unsigned;

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_32;
                                             Conversion_Ok : out Boolean)
   renames Unsigned_32_Number_Conversion_Utils.Hexadecimal_String_To_Unsigned;

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_16;
                                             Conversion_Ok : out Boolean)
   renames Unsigned_16_Number_Conversion_Utils.Hexadecimal_String_To_Unsigned;

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_8;
                                             Conversion_Ok : out Boolean)
   renames Unsigned_8_Number_Conversion_Utils.Hexadecimal_String_To_Unsigned;

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out System.Storage_Elements.Integer_Address;
                                             Conversion_Ok : out Boolean)
   renames Integer_Address_Number_Conversion_Utils.Hexadecimal_String_To_Unsigned;

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_64;
                                             Buffer : out String)
   renames Unsigned_64_Number_Conversion_Utils.Unsigned_To_Hexadecimal_String;

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_32;
                                             Buffer : out String)
   renames Unsigned_32_Number_Conversion_Utils.Unsigned_To_Hexadecimal_String;

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_16;
                                             Buffer : out String)
   renames Unsigned_16_Number_Conversion_Utils.Unsigned_To_Hexadecimal_String;

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_8;
                                             Buffer : out String)
   renames Unsigned_8_Number_Conversion_Utils.Unsigned_To_Hexadecimal_String;

   procedure Unsigned_To_Hexadecimal_String (Value : System.Storage_Elements.Integer_Address;
                                             Buffer : out String)
   renames Integer_Address_Number_Conversion_Utils.Unsigned_To_Hexadecimal_String;
end Utils.Number_Conversion;