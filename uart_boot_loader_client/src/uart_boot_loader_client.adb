--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary UART boot loader client
--
with Ada.Text_IO;
with Ada.Sequential_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Exceptions;
with Interfaces;
with Utils;

package body Uart_Boot_Loader_Client is
   package Binary_IO is new Ada.Sequential_IO (Interfaces.Unsigned_8);

   Packet_Size : constant := 1024;

   type Packet_Type is record
      Packet_Start_Control_Char : Character := ASCII.STX;
      Packet_Number : Interfaces.Unsigned_8 := 1;
      Packet_Number_Negated : Interfaces.Unsigned_8 := not 1;
      Data : Utils.Byte_Array_Type (1 .. Packet_Size);
      Checksum : Interfaces.Unsigned_32 := 0;
   end record;

   --
   --  Send binary file over UART using a modified version of the Xmodem protocol
   --
   procedure Send_File (File_Name : String)
   is
      use type Interfaces.Unsigned_8;

      procedure Send_Byte (Byte : Interfaces.Unsigned_8) is
         Stdout_Stream : constant Ada.Text_IO.Text_Streams.Stream_Access :=
            Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output);
      begin
         Interfaces.Unsigned_8'Write (Stdout_Stream, Byte);
      end Send_Byte;

      function Receive_Byte return Interfaces.Unsigned_8 is
         Stdin_Stream : constant Ada.Text_IO.Text_Streams.Stream_Access :=
            Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Input);
         Byte : Interfaces.Unsigned_8;
      begin
         Interfaces.Unsigned_8'Read (Stdin_Stream, Byte);
         return Byte;
      end Receive_Byte;

      procedure Send_Control_Char (Char : Character) is
      begin
         Send_Byte (Character'Pos (Char));
      end Send_Control_Char;

      procedure Send_Checksum (Checksum : Interfaces.Unsigned_32) is
         use Interfaces;
      begin
         Send_Byte (Unsigned_8 (Checksum and 16#ff#));
         Send_Byte (Unsigned_8 (Shift_Right (Checksum, 8) and 16#ff#));
         Send_Byte (Unsigned_8 (Shift_Right (Checksum, 16) and 16#ff#));
         Send_Byte (Unsigned_8 (Shift_Right (Checksum, 24)));
      end Send_Checksum;

      function Receive_Control_Char return Character is
         (Character'Val (Receive_Byte));

      procedure File_Transmission_Start_Handshake is
         Received_Char : Character;
      begin
         --  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         --     "File transmission start handshake");
         loop
            Send_Control_Char (ASCII.EOT);
            Received_Char := Receive_Control_Char;
            exit when Received_Char = ASCII.ACK;
         end loop;
      end File_Transmission_Start_Handshake;

      procedure File_Transmission_End_Handshake is
         Received_Char : Character;
      begin
         --  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         --     "File transmission end handshake");
         Send_Control_Char (ASCII.EOT);
         Received_Char := Receive_Control_Char;
         pragma Assert (Received_Char /= ASCII.ACK);
         Send_Control_Char (ASCII.EOT);
         Received_Char := Receive_Control_Char;
         pragma Assert (Received_Char = ASCII.ACK);
      end File_Transmission_End_Handshake;

      File_Obj : Binary_IO.File_Type;
      Packet : Packet_Type;
      Last_Index_Filled : Natural;
      Received_Char : Character;
      Packet_Retransmit_Count : Natural;
      Absolute_Packet_Number : Positive := 1;
      Actual_Data_Bytes_Transmitted : Natural := 0;
   begin
      Binary_IO.Open (File => File_Obj, Mode => Binary_IO.In_File,
                      Name => File_Name);

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         "Sending file: " & File_Name);
      File_Transmission_Start_Handshake;

      --  Transmit file:
      loop
         --  Read next file data block:
         Last_Index_Filled := 0;
         Packet.Checksum := 0;
         for I in Packet.Data'Range loop
            exit when Binary_IO.End_Of_File (File_Obj);
            Binary_IO.Read (File_Obj, Packet.Data (I));
            Last_Index_Filled := I;
         end loop;

         if Last_Index_Filled < Packet.Data'Last then
            for I in Last_Index_Filled + 1 .. Packet.Data'Last loop
               Packet.Data (I) := Character'Pos (ASCII.SUB);
            end loop;
         end if;

         Packet.Checksum := Utils.Compute_Checksum (Packet.Data);

         --  Transmit file data block:
         Packet_Retransmit_Count := 1;
         loop
            --  Transmit packet:
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error,
               "Packet" & Absolute_Packet_Number'Image &
               (if Packet_Retransmit_Count > 1 then
                  ", retries:" & Packet_Retransmit_Count'Image
               else
                  "") &
               "                                      " & ASCII.CR);
            Send_Control_Char (Packet.Packet_Start_Control_Char);
            Send_Byte (Packet.Packet_Number);
            Send_Byte (Packet.Packet_Number_Negated);
            for Byte of Packet.Data loop
               Send_Byte (Byte);
            end loop;

            Send_Checksum (Packet.Checksum);

            --  Wait for receiver's acknowledgment:
            Received_Char := Receive_Control_Char;
            exit when Received_Char = ASCII.ACK;
            if Received_Char = ASCII.CAN then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                  "File transmission cancelled by receiver");
               goto End_Of_Transmission;
            end if;

            loop
               exit when Received_Char = ASCII.NAK;
               --  For printing debug messages received from the server:
               --  Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Received_Char);
               Received_Char := Receive_Control_Char;
            end loop;

            Packet_Retransmit_Count := @ + 1;
         end loop;

         Absolute_Packet_Number := @ + 1;
         Actual_Data_Bytes_Transmitted := @ + Last_Index_Filled;
         exit when Last_Index_Filled < Packet_Size;

         Packet.Packet_Number := @ + 1;
         Packet.Packet_Number_Negated := not Packet.Packet_Number;
      end loop;

      File_Transmission_End_Handshake;

<<End_Of_Transmission>>
      Binary_IO.Close (File_Obj);
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         "*** File transmission completed successfully -" &
         Actual_Data_Bytes_Transmitted'Image & " bytes");

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               Ada.Exceptions.Exception_Message (E));
         raise;
   end Send_File;
end Uart_Boot_Loader_Client;
