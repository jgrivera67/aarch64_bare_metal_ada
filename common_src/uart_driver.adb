--
--  Copyright (c) 2022-2023, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary UART driver for ARM PL011
--

package body Uart_Driver is

   use type Bit_Sized_Integer_Types.Bit_Type;

   procedure Put_Char (C : Character) is
   begin
      Send_Byte (Interfaces.Unsigned_8 (Character'Pos (C)));
   end Put_Char;

   procedure Send_Byte (Byte : Interfaces.Unsigned_8) is
      UARTFR_Value : UARTFR_Register;
      UARTDR_Value : UARTDR_Register;
   begin
      loop
         UARTFR_Value := UART0_Periph.UARTFR;
         exit when UARTFR_Value.TXFF = 0;
      end loop;

      UARTDR_Value.DATA := Byte;
      UART0_Periph.UARTDR := UARTDR_Value;
   end Send_Byte;

   function Get_Char return Character is
      (Character'Val (Receive_Byte));

   function Receive_Byte return Interfaces.Unsigned_8 is
      UARTFR_Value : UARTFR_Register;
      UARTDR_Value : UARTDR_Register;
   begin
      loop
         UARTFR_Value := UART0_Periph.UARTFR;
         exit when UARTFR_Value.RXFE = 0;
      end loop;

      UARTDR_Value := UART0_Periph.UARTDR;

      return UARTDR_Value.DATA;
   end Receive_Byte;

   function Receive_Byte_If_Any return Maybe_Byte_Type is
      Result : Maybe_Byte_Type;
      UARTFR_Value : UARTFR_Register;
      UARTDR_Value : UARTDR_Register;
   begin
      UARTFR_Value := UART0_Periph.UARTFR;
      if UARTFR_Value.RXFE = 0 then
         UARTDR_Value := UART0_Periph.UARTDR;
         Result := (Valid => True,
                    Byte => UARTDR_Value.DATA);
      else
         Result.Valid := False;
      end if;

      return Result;
   end Receive_Byte_If_Any;

end Uart_Driver;
