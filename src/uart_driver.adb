--
--  Copyright (c) 2022-2023, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary UART driver for ARM PL011
--

with Interrupt_Controller_Driver;
with CPU.Interrupt_Handling;
with Utils.Runtime_Log;

package body Uart_Driver is

   use type Bit_Sized_Integer_Types.Bit_Type;

   procedure Initialize_Uart (Rx_Interrupt_Callback_Pointer : Uart_Rx_Interrupt_Callback_Pointer_Type;
                              Rx_Interrupt_Callback_Arg : System.Address;
                              Baud_Rate : Interfaces.Unsigned_32 := Default_Baud_Rate)
   is
      procedure Calculate_Divisors (Baud_Rate : Interfaces.Unsigned_32;
                                    Integer_Div : out Interfaces.Unsigned_16;
                                    Fractional_Div : out Bit_Sized_Integer_Types.Six_Bits_Type) is
         use type Interfaces.Unsigned_32;
      begin
         --  F_UARTCLK / (16 * Baudrate)
         Integer_Div := Interfaces.Unsigned_16 (Board.Uart_Clock_Frequency_Hz / (16 * Baud_Rate));
         Fractional_Div := Bit_Sized_Integer_Types.Six_Bits_Type (0);
      end Calculate_Divisors;

      UARTIBRD_Value : UARTIBRD_Register;
      UARTFBRD_Value : UARTFBRD_Register;
      UARTLCR_H_Value : UARTLCR_H_Register;
      UARTCR_Value : UARTCR_Register;
      UARTIMSC_Value : UARTIMSC_Register;
      --  UARTIFLS_Value : UARTIFLS_Register;

      procedure Disable_Uart is
      begin
         --  Disable UART Tx/Rx and UART peripheral itself:
         UARTCR_Value := UART0_Periph.UARTCR;
         UARTCR_Value.TXE := 2#0#;
         UARTCR_Value.RXE := 2#0#;
         UARTCR_Value.UARTEN := 2#0#;
         UART0_Periph.UARTCR := UARTCR_Value;

         UARTLCR_H_Value := UART0_Periph.UARTLCR_H;
         UARTLCR_H_Value.FEN := 2#0#;
         UART0_Periph.UARTLCR_H := UARTLCR_H_Value;
      end Disable_Uart;

      procedure Enable_Uart is
      begin
         --  Enable UART Tx/Rx and UART peripheral itself:
         UARTCR_Value := UART0_Periph.UARTCR;
         UARTCR_Value.TXE := 2#1#;
         UARTCR_Value.RXE := 2#1#;
         UARTCR_Value.UARTEN := 2#1#;
         UART0_Periph.UARTCR := UARTCR_Value;
      end Enable_Uart;

      use CPU.Interrupt_Handling;

   begin
      Uart_Device.Rx_Interrupt_Callback_Pointer := Rx_Interrupt_Callback_Pointer;
      Uart_Device.Rx_Interrupt_Callback_Arg := Rx_Interrupt_Callback_Arg;
      Disable_Uart;

      --  Disable all interrupts:
      UART0_Periph.UARTIMSC := (others => 0);

      --  Clear any pending interrupt:
      UART0_Periph.UARTICR := (others => 1);

      --  Set baud rate:
      Calculate_Divisors (Baud_Rate, UARTIBRD_Value.BAUD_DIVINT, UARTFBRD_Value.BAUD_DIVFRAC);

      UART0_Periph.UARTIBRD := UARTIBRD_Value;
      UART0_Periph.UARTFBRD := UARTFBRD_Value;

      --  Configure data frame format to be 8-N-1 (8 data bits, no parity, 1 stop bit)
      UARTLCR_H_Value := UART0_Periph.UARTLCR_H;
      UARTLCR_H_Value.WLEN := 2#11#;
      UARTLCR_H_Value.STP2 := 2#0#;
      UARTLCR_H_Value.PEN := 2#0#;
      UART0_Periph.UARTLCR_H := UARTLCR_H_Value;

      --
      --  Disable Rx/Tx FIFOs, so that we can receive Rx interrupts after receiving
      --  just one byte:
      --
      --  NOTE: Ideally we want to disable just the Rx FIFO, but there is
      --  no way to disable just one of the FIFOs. With the Rx FIFO enabled
      --  the smallest granularity to receive Rx interrupts 1/8 of the Rx FIFO
      --  size. Since the size of the FIFO is 32 bytes, 4 bytes would need to
      --  be received, before an Rx interrupt fires.
      --
      --  UARTIFLS_Value := UART0_Periph.UARTIFLS;
      --  UARTIFLS_Value.RXIFLSEL := 2#000#; --  Rx FIFO at least 1/8 full
      --  UART0_Periph.UARTIFLS := UARTIFLS_Value;
      UARTLCR_H_Value := UART0_Periph.UARTLCR_H;
      UARTLCR_H_Value.FEN := 2#0#;
      UART0_Periph.UARTLCR_H := UARTLCR_H_Value;

      --  Enable Rx interrupt:
      UARTIMSC_Value := UART0_Periph.UARTIMSC;
      UARTIMSC_Value.RXIM := 2#1#;
      UART0_Periph.UARTIMSC := UARTIMSC_Value;

      --  Configure UART interrupt in the GIC:
      Interrupt_Controller_Driver.Configure_External_Interrupt (
         External_Interrupt_Id =>  UART0_Interrupt_Id,
         Priority => Interrupt_Priorities (UART0_Interrupt_Id),
         Cpu_Interrupt_Line => Interrupt_Controller_Driver.Cpu_Interrupt_Irq,
         Trigger_Mode => Interrupt_Controller_Driver.Interrupt_Level_Sensitive,
         Interrupt_Handler_Entry_Point => Uart_Rx_Interrupt_Handler'Access,
         Interrupt_Handler_Arg => Uart_Device'Address);

      --  Enable UART interrupt in the GIC:
      Interrupt_Controller_Driver.Enable_External_Interrupt (UART0_Interrupt_Id);
      Enable_Uart;
      Utils.Runtime_Log.Log_Info_Msg ("UART initialized");
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

   procedure Flush_Output is
      UARTFR_Value : UARTFR_Register;
   begin
      loop
         UARTFR_Value := UART0_Periph.UARTFR;
         exit when UARTFR_Value.TXFE = 1;
      end loop;
   end Flush_Output;

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

   function Input_Interrupt_Enabled return Boolean is
      UARTIMSC_Value : constant UARTIMSC_Register := UART0_Periph.UARTIMSC;
   begin
      return UARTIMSC_Value.RXIM = 2#1#;
   end Input_Interrupt_Enabled;

   procedure Uart_Rx_Interrupt_Handler (Arg : System.Address) is
      Uart_Device_Obj : Uart_Device_Type with Import, Address => Arg;
      UARTFR_Value : UARTFR_Register;
      UARTDR_Value : UARTDR_Register;
   begin
      pragma  Assert (Uart_Device_Obj.Rx_Interrupt_Callback_Pointer /= null);
      loop
         UARTFR_Value := UART0_Periph.UARTFR;
         exit when UARTFR_Value.RXFE = 1;
         UARTDR_Value := UART0_Periph.UARTDR;
         Uart_Device_Obj.Rx_Interrupt_Callback_Pointer (Uart_Device_Obj.Rx_Interrupt_Callback_Arg,
                                                        Byte_Received => UARTDR_Value.DATA);
      end loop;
   end Uart_Rx_Interrupt_Handler;

end Uart_Driver;
