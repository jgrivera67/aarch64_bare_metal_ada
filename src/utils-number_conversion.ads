--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Storage_Elements;

package Utils.Number_Conversion
   with SPARK_Mode => On
is
   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_64;
                                             Conversion_Ok : out Boolean);

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_32;
                                             Conversion_Ok : out Boolean);

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_16;
                                             Conversion_Ok : out Boolean);

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out Interfaces.Unsigned_8;
                                             Conversion_Ok : out Boolean);

   procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                             Value : out System.Storage_Elements.Integer_Address;
                                             Conversion_Ok : out Boolean);

   subtype Unsigned_64_Hexadecimal_String_Type is String (1 .. 16);
   subtype Unsigned_32_Hexadecimal_String_Type is String (1 .. 8);
   subtype Unsigned_16_Hexadecimal_String_Type is String (1 .. 4);
   subtype Unsigned_8_Hexadecimal_String_Type is String (1 .. 2);

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_64;
                                             Buffer : out String);

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_32;
                                             Buffer : out String);

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_16;
                                             Buffer : out String);

   procedure Unsigned_To_Hexadecimal_String (Value : Interfaces.Unsigned_8;
                                             Buffer : out String);

   procedure Unsigned_To_Hexadecimal_String (Value : System.Storage_Elements.Integer_Address;
                                             Buffer : out String);

   subtype Unsigned_32_Decimal_String_Type is String (1 .. 10);

   procedure Unsigned_To_Decimal_String (Value : Interfaces.Unsigned_32;
                                         Buffer : out String;
                                         Actual_Length : out Positive;
                                         Add_Leading_Zeros : Boolean := False);

private

   -----------------------------------------------------------------------------
   --  Generic numeric conversion utilities
   -----------------------------------------------------------------------------
   generic
      type Unsigned_Type is mod <>;
      with function Shift_Right_Func (Value : Unsigned_Type; Shift : Natural) return Unsigned_Type;
      with function Shift_Left_Func (Value : Unsigned_Type; Shift : Natural) return Unsigned_Type;
   package Generic_Number_Conversion_Utils
   is
      procedure Unsigned_To_Hexadecimal_String (Value : Unsigned_Type;
                                                Buffer : out String);

      procedure Hexadecimal_String_To_Unsigned (Hexadecimal_Str : String;
                                                Value : out Unsigned_Type;
                                                Conversion_Ok : out Boolean);
   end Generic_Number_Conversion_Utils;
end Utils.Number_Conversion;
