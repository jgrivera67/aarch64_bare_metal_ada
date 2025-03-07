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

   procedure Initialize_Uart (Baud_Rate : Interfaces.Unsigned_32;
                              UART_Clock_Frequency_Hz : Interfaces.Unsigned_32) is

      procedure Calculate_Divisors (Baudrate : Interfaces.Unsigned_32;
                                    Integer_Div : out Interfaces.Unsigned_16;
                                    Fractional_Div : out Bit_Sized_Integer_Types.Six_Bits_Type) is
         use type Interfaces.Unsigned_32;
         --  64 * F_UARTCLK / (16 * Baudrate) = 4 * F_UARTCLK / Baudrate
         Divider : constant Interfaces.Unsigned_32 :=
            4 * UART_Clock_Frequency_Hz / Baudrate;
      begin
         Integer_Div := Interfaces.Unsigned_16 (Interfaces.Shift_Left (Divider, 6) and 16#ffff#);
         Fractional_Div := Bit_Sized_Integer_Types.Six_Bits_Type (Divider and 2#111111#);
      end Calculate_Divisors;

      UARTIBRD_Value : UARTIBRD_Register;
      UARTFBRD_Value : UARTFBRD_Register;
      UARTLCR_H_Value : UARTLCR_H_Register;
      UARTCR_Value : UARTCR_Register;
      UARTIMSC_Value : UARTIMSC_Register;
      UARTICR_Value : UARTICR_Register;
      UARTIFLS_Value : UARTIFLS_Register;
   begin
      --  Set baud rate:
      Calculate_Divisors (Baud_Rate, UARTIBRD_Value.BAUD_DIVINT, UARTFBRD_Value.BAUD_DIVFRAC);

      UART0_Periph.UARTIBRD := UARTIBRD_Value;
      UART0_Periph.UARTFBRD := UARTFBRD_Value;

      --  Configure data frame format to be 8-N-1 (8 data bits, no parity, 1 stop bit)
      UARTLCR_H_Value.WLEN := 2#11#;
      UARTLCR_H_Value.STP2 := 2#0#;
      UARTLCR_H_Value.PEN := 2#0#;
      UARTLCR_H_Value.FEN := 2#1#;
      UART0_Periph.UARTLCR_H := UARTLCR_H_Value;

      --  Disable (mask) all interrupts:
      UARTIMSC_Value := (others => 2#0#);
      UART0_Periph.UARTIMSC := UARTIMSC_Value;

      --  Clear any pending interrupt:
      UARTICR_Value := (others => 2#1#);
      UART0_Periph.UARTICR := UARTICR_Value;

      --
      --  Disable UART Rx FIFO
      --
      UARTIFLS_Value := UART0_Periph.UARTIFLS;
      UARTIFLS_Value.RXIFLSEL := 2#000#;
      UART0_Periph.UARTIFLS := UARTIFLS_Value;

      --  Enable UART Tx/Rx and UART peripheral itself:
      UARTCR_Value := UART0_Periph.UARTCR;
      UARTCR_Value.TXE := 2#1#;
      UARTCR_Value.RXE := 2#1#;
      UARTCR_Value.UARTEN := 2#1#;
      UART0_Periph.UARTCR := UARTCR_Value;
   end Initialize_Uart;

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
