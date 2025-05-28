--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

with Bit_Sized_Integer_Types;
with CPU;

package Utils.Bit_Manipulation
   with SPARK_Mode => On
is
   use Bit_Sized_Integer_Types;

   type Bit_Array_Type is array (Natural range <>) of Bit_Type
      with Component_Size => 1;

   use CPU;

   type Bit_Index_Type is mod Cpu_Register_Type'Size;

   function Bit_Mask (Bit_Index : Bit_Index_Type) return Cpu_Register_Type is
    (Cpu_Register_Type (2 ** Natural (Bit_Index)));

   function Is_Value_Power_Of_Two (Value : Cpu_Register_Type) return Boolean is
      (Value /= 0 and then
       (Value and (Value - 1)) = 0);
end Utils.Bit_Manipulation;