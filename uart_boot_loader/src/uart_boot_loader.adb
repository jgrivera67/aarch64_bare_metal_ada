--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with Uart_Driver;
with Utils;
with System.Storage_Elements;
with System.Machine_Code;

package body Uart_Boot_Loader is

   procedure Load_Image_Over_Uart (Load_Address : System.Address) is
   begin
      Receive_File (Load_Address);

      --
      --  Send dummy bytes to give time to UART terminal emulator to
      --  switch from file transfer mode to console mode.
      --
      for I in 1 .. 64 loop
         Uart_Driver.Put_Char (ASCII.NUL);
      end loop;

      Jump_To_Image_Reset_Handler (Load_Address);
   end Load_Image_Over_Uart;

   --
   --  Receive binary file over UART using a modified version of the Xmodem protocol
   --
   procedure Receive_File (Load_Address : System.Address) is
      use System.Storage_Elements;
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_32;

      function Receive_Packet_Data (Load_Address : System.Address;
                                    Packet_Size : Interfaces.Unsigned_16)
                                    return Maybe_Checksum_Type is
         Packet_Buffer : Utils.Byte_Array_Type (1 .. Positive (Packet_Size)) with
                            Address => Load_Address;
         Maybe_Received_Byte : Uart_Driver.Maybe_Byte_Type;
         Maybe_Checksum : Maybe_Checksum_Type;
      begin
         for I in Packet_Buffer'Range loop
            Maybe_Received_Byte := Utils.Receive_Byte_With_Timeout (Receive_Timeout_Usec);
            if Maybe_Received_Byte.Valid then
               Packet_Buffer (I) := Maybe_Received_Byte.Byte;
            else
               return Maybe_Checksum;
            end if;
         end loop;

         Maybe_Checksum := (Valid => True,
                            Checksum => Utils.Compute_Checksum (Packet_Buffer));
         return Maybe_Checksum;
      end Receive_Packet_Data;

      function Receive_Checksum return Maybe_Checksum_Type is
         use Interfaces;
         Maybe_Received_Byte : Uart_Driver.Maybe_Byte_Type;
         Maybe_Checksum : Maybe_Checksum_Type;
      begin
            Maybe_Checksum.Checksum := 0;
            for I in 0 .. 3 loop
               Maybe_Received_Byte := Utils.Receive_Byte_With_Timeout (Receive_Timeout_Usec);
               if Maybe_Received_Byte.Valid then
                  Maybe_Checksum.Checksum :=
                     Shift_Left (Unsigned_32 (Maybe_Received_Byte.Byte), I * 8) or @;
               else
                  return Maybe_Checksum;
               end if;
            end loop;

            Maybe_Checksum.Valid := True;
            return Maybe_Checksum;
      end Receive_Checksum;

      procedure Handle_EOT is
      begin
         loop
            Uart_Driver.Put_Char (ASCII.NAK);
            exit when Uart_Driver.Get_Char = ASCII.EOT;
         end loop;

         Uart_Driver.Put_Char (ASCII.ACK);
      end Handle_EOT;

      Packet_Size : Interfaces.Unsigned_16 := 0;
      Expected_Packet_Num : Interfaces.Unsigned_8 := 1;
      Received_Packet_Num : Interfaces.Unsigned_8 := 0;
      Received_Packet_Num_Complement : Interfaces.Unsigned_8 := 0;
      Maybe_Computed_Checksum : Maybe_Checksum_Type;
      Maybe_Received_Checksum : Maybe_Checksum_Type;
      Load_Cursor_Address : System.Address := Load_Address;
      Received_Byte : Interfaces.Unsigned_8;
   begin
      --  Initial handshake with sender:
      loop
         Received_Byte := Uart_Driver.Receive_Byte;
         if Character'Val (Received_Byte) = ASCII.EOT then
            Handle_EOT;
            exit;
         end if;
      end loop;

      --  Main packet receive loop:
      loop
         Received_Byte := Uart_Driver.Receive_Byte;
         case Character'Val (Received_Byte) is
            when ASCII.SOH =>
               Packet_Size := 128;
            when ASCII.STX =>
               Packet_Size := 1024;
            when ASCII.EOT =>
               Handle_EOT;
               exit;
            when others =>
               Uart_Driver.Put_Char (ASCII.NAK);
               goto Next_Iteration;
         end case;

         Received_Packet_Num := Uart_Driver.Receive_Byte;
         Received_Packet_Num_Complement := Uart_Driver.Receive_Byte;
         if Received_Packet_Num /= Expected_Packet_Num or else
            Received_Packet_Num_Complement /= not Received_Packet_Num
         then
            Uart_Driver.Put_Char (ASCII.NAK);
            goto Next_Iteration;
         end if;

         Maybe_Computed_Checksum := Receive_Packet_Data (Load_Cursor_Address, Packet_Size);
         if not Maybe_Computed_Checksum.Valid then
            --  Timeout receiving packet data:
            Uart_Driver.Put_Char (ASCII.NAK);
            goto Next_Iteration;
         end if;

         Maybe_Received_Checksum := Receive_Checksum;
         if not Maybe_Received_Checksum.Valid then
            --  Timeout receiving checksum data:
            Uart_Driver.Put_Char (ASCII.NAK);
            goto Next_Iteration;
         end if;

         pragma Assert (Maybe_Computed_Checksum.Valid and then
                        Maybe_Received_Checksum.Valid);

         if Maybe_Received_Checksum.Checksum /= Maybe_Computed_Checksum.Checksum then
            Uart_Driver.Put_Char (ASCII.NAK);
            goto Next_Iteration;
         end if;

         Load_Cursor_Address := To_Address (To_Integer (Load_Cursor_Address) +
                                            Integer_Address (Packet_Size));
         Expected_Packet_Num := @ + 1;
         Uart_Driver.Put_Char (ASCII.ACK);

      <<Next_Iteration>>
      end loop;
   end Receive_File;

   procedure Jump_To_Image_Reset_Handler (Reset_Handler_Address : System.Address) is
   begin
      System.Machine_Code.Asm (
          "dmb sy" & ASCII.LF &
          "br %0",
           Inputs => System.Address'Asm_Input ("r", Reset_Handler_Address),  --  %0
           Volatile => True);
   end Jump_To_Image_Reset_Handler;

end Uart_Boot_Loader;
