--
--  Copyright (c) 2025, German Rivera
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  Self-hosted debugger GDB server
--
--  NOTE: For GDB remote protocol packet formats see
--  https://sourceware.org/gdb/onlinedocs/gdb/Packets.html
--

with CPU;
with CPU.Memory_Protection;
with Utils.Number_Conversion;
with Utils.Runtime_Log;

package body Gdb_Server is
   use Utils.Number_Conversion;
   use Utils.Runtime_Log;

   procedure Run_Gdb_Server (
      Debug_Event : CPU.Self_Hosted_Debug.Debug_Event_Type;
      Current_PC : in out System.Address)
   is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Gdb_Server_Obj : Gdb_Server_Type renames Gdb_Server_Objects (Cpu_Id);
      Data_Last_Index : Valid_Gdb_Packet_Data_Index_Type;
   begin
      --  Block access to the UART console from other CPUs while the GDB server is running:
      Utils.Lock_Console (Print_Cpu => False);

      if Gdb_Server_Obj.Gdb_Attached then
         --  Tell the GDB client that the target is halted at a debug exception:
         Send_Gdb_Stop_Reply_Packet (Gdb_Server_Obj);
      else
         Log_Info_Msg_Begin ("Entering self-hosted debugger on ");
         Log_Info_Msg_Part (Debug_Event'Image);
         Log_Info_Msg_Part (" at PC ");
         Log_Info_Value_Hexadecimal (Interfaces.Unsigned_64 (To_Integer (Current_PC)));
         Log_Info_Msg_End;
         CPU.Self_Hosted_Debug.Enable_Self_Hosted_Debugging;
         Gdb_Server_Obj.Self_Hosted_Debug_Capabilities :=
            CPU.Self_Hosted_Debug.Get_Self_Hosted_Debug_Capabilities;
      end if;

      Gdb_Server_Obj.Enter_Debug_Event := Debug_Event;
      Gdb_Server_Obj.Current_PC := Current_PC;
      Gdb_Server_Obj.Resume_Target := False;
      Gdb_Server_Obj.Gdb_Server_Aborted := False;
      Gdb_Server_Obj.Running := True;

      loop
         Receive_Gdb_Packet (Gdb_Server_Obj);
         exit when Gdb_Server_Obj.Gdb_Server_Aborted;
         Data_Last_Index := Valid_Gdb_Packet_Data_Index_Type (Gdb_Server_Obj.Gdb_Packet_Data_Length);
         Process_Incoming_Gdb_Packet (
            Gdb_Server_Obj, Gdb_Server_Obj.Gdb_Packet_Data_Buffer (1),
            Gdb_Server_Obj.Gdb_Packet_Data_Buffer (2 .. Data_Last_Index));
         exit when Gdb_Server_Obj.Resume_Target;
      end loop;

      Gdb_Server_Obj.Running := False;
      if not Gdb_Server_Obj.Gdb_Attached or else Gdb_Server_Obj.Gdb_Server_Aborted then
         CPU.Self_Hosted_Debug.Disable_Self_Hosted_Debugging;
         Log_Info_Msg_Begin ("Exiting self-hosted debugger at PC ");
         Log_Info_Value_Hexadecimal (Interfaces.Unsigned_64 (To_Integer (Current_PC)));
         Log_Info_Msg_End;
      end if;

      Utils.Unlock_Console;
   end Run_Gdb_Server;

   --
   --  Receive the next GDB packet over UART, in ASCII format: `$<data>#<checksum: 2 ASCII hex digits>`
   --
   procedure Receive_Gdb_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
      function Get_Next_Printable_Char return Character is
         Byte_Received : Character;
      begin
         loop
            Byte_Received := Utils.Get_Char;
            exit when Byte_Received in ' ' .. '~' | Utils.Ctrl_C;
         end loop;
         return Byte_Received;
      end Get_Next_Printable_Char;

      procedure Abort_Gdb_Server (Gdb_Server_Obj : in out Gdb_Server_Type) is
      begin
         Log_Info_Msg ("Aborting GDB server ...");
         Gdb_Server_Obj.Gdb_Server_Aborted := True;
         Dump_Runtime_Log;
      end Abort_Gdb_Server;

      procedure Try_Receive_Gdb_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                        Receive_Ok : out Boolean) is
         use type Interfaces.Unsigned_8;
         Recv_Checksum_Digits : String (1 .. 2);
         Recv_Checksum : Interfaces.Unsigned_8;
         Conversion_Ok : Boolean := False;
         Computed_Checksum : Interfaces.Unsigned_8 := 0;
         Buffer : String renames Gdb_Server_Obj.Gdb_Packet_Data_Buffer;
         Gdb_Packet_Data_Length : Gdb_Packet_Data_Length_Type := 0;
         C : Character;
      begin
         Gdb_Server_Obj.Gdb_Packet_Data_Length := 0;
         Receive_Ok := False;
         loop
            C := Get_Next_Printable_Char;
            exit when C in '$' | Utils.Ctrl_C;
         end loop;

         if C = Utils.Ctrl_C then
            --  GDB server aborted by user:
            Utils.Print_String (ASCII.LF & "^C" & ASCII.LF);
            Abort_Gdb_Server (Gdb_Server_Obj);
            return;
         end if;

         for I in Buffer'Range loop
            C := Get_Next_Printable_Char;
            exit when C = '#';
            Buffer (I) := C;
            Gdb_Packet_Data_Length := @ + 1;
            Computed_Checksum := @ + Character'Pos (C);
         end loop;

         if C /= '#' then
            return;
         end if;

         Recv_Checksum_Digits (1) := Get_Next_Printable_Char;
         Recv_Checksum_Digits (2) := Get_Next_Printable_Char;
         Utils.Number_Conversion.Hexadecimal_String_To_Unsigned (Recv_Checksum_Digits,
                                                                 Recv_Checksum,
                                                                 Conversion_Ok);
         if not Conversion_Ok then
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
            return;
         end if;

         if Computed_Checksum /= Recv_Checksum then
            --  Bad packet. Request packet re-transmission:
            C := '-';
            Utils.Put_Char ('-');
            return;
         end if;

         --
         --  Acknowledge packet:
         --
         Utils.Put_Char ('+');
         Gdb_Server_Obj.Gdb_Packet_Data_Length := Gdb_Packet_Data_Length;
         Receive_Ok := True;
      end Try_Receive_Gdb_Packet;

      Receive_Ok : Boolean := False;

   begin -- Receive_Gdb_Packet
      loop
         Try_Receive_Gdb_Packet (Gdb_Server_Obj, Receive_Ok);
         exit when Receive_Ok or else Gdb_Server_Obj.Gdb_Server_Aborted;
      end loop;

      if Debug_On and then not Gdb_Server_Obj.Gdb_Server_Aborted then
         Print_Gdb_Packet_Data (Gdb_Server_Obj, "Received");
      end if;
   end Receive_Gdb_Packet;

   --
   --  Send a GDB packet over UART, in ASCII format: `$<data>#<checksum: 2 ASCII hex digits>`
   --
   procedure Send_Gdb_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
   begin
      Send_Gdb_Packet_Fragment (Gdb_Server_Obj,
                                Is_Packet_First_Fragment => True,
                                Is_Packet_Last_Fragment => True);
   end Send_Gdb_Packet;

   procedure Send_Gdb_Packet_Fragment (Gdb_Server_Obj : in out Gdb_Server_Type;
                                       Is_Packet_First_Fragment : Boolean;
                                       Is_Packet_Last_Fragment : Boolean) is
      use type Interfaces.Unsigned_8;
      Data_Length : Gdb_Packet_Data_Length_Type renames Gdb_Server_Obj.Gdb_Packet_Data_Length;
      Checksum_Digits : String (1 .. 2);
      Computed_Checksum : Interfaces.Unsigned_8;
   begin
      if Is_Packet_First_Fragment then
         Utils.Put_Char ('$');
         Gdb_Server_Obj.Computed_Checksum := 0;
      end if;

      Computed_Checksum := Gdb_Server_Obj.Computed_Checksum;
      if Data_Length /= 0 then
         declare
            Data_Buffer : String renames
               Gdb_Server_Obj.Gdb_Packet_Data_Buffer (1 .. Valid_Gdb_Packet_Data_Index_Type (Data_Length));
         begin
            --
            --  Compute checksum:
            --
            for C of Data_Buffer loop
               Computed_Checksum := @ + Character'Pos (C);
            end loop;

            --  Transmit packet payload:
            Utils.Print_String (Data_Buffer);
         end;
      else
         pragma Assert (Is_Packet_First_Fragment);
         pragma Assert (Is_Packet_Last_Fragment);
      end if;

      if Is_Packet_Last_Fragment then
         Utils.Put_Char ('#');
         Utils.Number_Conversion.Unsigned_To_Hexadecimal_String (Computed_Checksum,
                                                                 Checksum_Digits);
         Utils.Print_String (Checksum_Digits);
      else
         Gdb_Server_Obj.Computed_Checksum := Computed_Checksum;
      end if;

      if Debug_On then
         Print_Gdb_Packet_Data (Gdb_Server_Obj, "Sent");
      end if;
   end Send_Gdb_Packet_Fragment;

   procedure Print_Gdb_Packet_Data (Gdb_Server_Obj : Gdb_Server_Type;
                                    Label : String) is
      Data_Length : Gdb_Packet_Data_Length_Type renames Gdb_Server_Obj.Gdb_Packet_Data_Length;
   begin
      Log_Debug_Msg_Begin (Label);
      if Data_Length /= 0 then
         Log_Debug_Msg_Part (" GDB packet: '");
         Log_Debug_Msg_Part (Gdb_Server_Obj.Gdb_Packet_Data_Buffer (
            1 .. Valid_Gdb_Packet_Data_Index_Type (Data_Length)));
         Log_Debug_Msg_End ("'");
      else
         Log_Debug_Msg_End (" empty GDB packet");
      end if;
   end Print_Gdb_Packet_Data;

   procedure Process_Incoming_Gdb_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                          Packet_Command : Character;
                                          Packet_Args : String) is
   begin
      case Packet_Command is
         when 'D' =>
            Process_Uppercase_D_Packet (Gdb_Server_Obj);

         when 'c' =>
            Process_Lowercase_C_Packet (Gdb_Server_Obj);

         when 'g' =>
            Process_Lowercase_G_Packet (Gdb_Server_Obj);

         when 'k' =>
            Process_Lowercase_K_Packet (Gdb_Server_Obj);

         when 'm' =>
            Process_Lowercase_M_Packet (Gdb_Server_Obj, Packet_Args);

         when 'M' =>
            Process_Uppercase_M_Packet (Gdb_Server_Obj, Packet_Args);

         when 'p' =>
            Process_Lowercase_P_Packet (Gdb_Server_Obj, Packet_Args);

         when 'q' =>
            Process_Lowercase_Q_Packet (Gdb_Server_Obj, Packet_Args);

         when 'Q' =>
            Process_Uppercase_Q_Packet (Gdb_Server_Obj, Packet_Args);

         when 'H' =>
            Process_Uppercase_H_Packet (Gdb_Server_Obj, Packet_Args);

         when 'T' =>
            Process_Uppercase_T_Packet (Gdb_Server_Obj, Packet_Args);

         when 'v' =>
            Process_Lowercase_V_Packet (Gdb_Server_Obj, Packet_Args);

         when 'X' =>
            Process_Uppercase_X_Packet (Gdb_Server_Obj, Packet_Args);

         when 'Z' =>
            Process_Uppercase_Z_Packet (Gdb_Server_Obj, Packet_Args);

         when 'z' =>
            Process_Lowercase_Z_Packet (Gdb_Server_Obj, Packet_Args);

         when '?' =>
            Process_Question_Mark_Packet (Gdb_Server_Obj, Packet_Args);

         when others =>
            null;
      end case;
   end Process_Incoming_Gdb_Packet;

   procedure Process_Uppercase_D_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
   begin
      Gdb_Server_Obj.Gdb_Packet_Data_Buffer (1 .. 2) := "OK";
      Gdb_Server_Obj.Gdb_Packet_Data_Length := 2;
      Send_Gdb_Packet (Gdb_Server_Obj);

      --  Resume target and detach from gdb:
      Gdb_Server_Obj.Resume_Target := True;
      Gdb_Server_Obj.Gdb_Attached := False;
   end Process_Uppercase_D_Packet;

   procedure Process_Lowercase_C_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
   begin
      if Gdb_Server_Obj.Enter_Debug_Event /= Dummy_Debug_Event then
         Send_Uppercase_S_Packet (Gdb_Server_Obj, Trap_Signal);
      else
         --  Resume target but keep attached to GDB:
         Gdb_Server_Obj.Resume_Target := True;
         Gdb_Server_Obj.Gdb_Attached := True;
      end if;
   end Process_Lowercase_C_Packet;

   procedure Process_Lowercase_G_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
   begin
      if Gdb_Server_Obj.Current_Hg_Thread_Id /= To_Integer (System.Null_Address) then
         --  RTOS threads not supported yet
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E5f"); --  errno 95 (ENOTSUP)
            return;
      end if;

      declare
         use CPU.Interrupt_Handling;
         use type CPU.Cpu_Register_Type;
         Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
         Interrupt_Nesting : constant Interrupt_Nesting_Type := Get_Cpu_Interrupt_Nesting (Cpu_Id);
         Stack_Pointer : constant System.Address :=
            Get_Interrupt_Nesting_Stack_Pointer (Interrupt_Nesting);
         Cpu_Context : constant Cpu_Context_Type with Import, Address => Stack_Pointer;
         Buffer_Cursor : Gdb_Packet_Data_Index_Type := 1;
         Big_Endian_Reg_Value : CPU.Cpu_Register_Type;
      begin
         pragma Assert (Cpu_Context.Registers (SP) = CPU.Cpu_Register_Type (To_Integer (Stack_Pointer)));
         Gdb_Server_Obj.Enter_Debug_Event := Dummy_Debug_Event;
         for Gdb_Reg_Id in Gdb_Cpu_Register_Id_Type loop
            --
            --  NOTE: Register values must be transmitted in target byte order,
            --  which in our case is little-endian. So, to obtain the right order
            --  of hex digit pairs, we need to byte convert to big endian before converting
            --  to hex digit pairs. This way the first hex-pair transmitted is the one
            --  corresponding to the least significant byte of the original value.
            --
            declare
               use Interfaces;
               Cpu_Reg_Id : constant Cpu_Register_Id_Type :=
                  Gdb_Cpu_Register_Id_To_Cpu_Register_Id (Gdb_Reg_Id);
            begin
               Big_Endian_Reg_Value :=
                  CPU.Convert_To_Big_Endian (Cpu_Context.Registers (Cpu_Reg_Id));
               if Gdb_Reg_Id = Gdb_CPSR then
                  Unsigned_To_Hexadecimal_String (
                     Unsigned_32 (
                        Unsigned_64 (Big_Endian_Reg_Value) and Unsigned_64 (Interfaces.Unsigned_32'Last)),
                     Gdb_Server_Obj.Gdb_Packet_Data_Buffer (Buffer_Cursor .. Buffer_Cursor + 7));
                  Buffer_Cursor := @ + 8;
               else
                  Unsigned_To_Hexadecimal_String (
                     Unsigned_64 (Big_Endian_Reg_Value),
                     Gdb_Server_Obj.Gdb_Packet_Data_Buffer (Buffer_Cursor .. Buffer_Cursor + 15));
                  Buffer_Cursor := @ + 16;
               end if;
            end;
         end loop;

         Gdb_Server_Obj.Gdb_Packet_Data_Length := Gdb_Packet_Data_Length_Type (Buffer_Cursor - 1);
         Send_Gdb_Packet (Gdb_Server_Obj);
      end;
   end Process_Lowercase_G_Packet;

   procedure Process_Uppercase_H_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      use Utils;
   begin
      if Equal_Strings (Packet_Args, "g0") then
         --  TODO: Set Gdb_Server_Obj.Current_Hg_Thread_Id when RTOS support is added.
         Send_Gdb_Ok_Packet (Gdb_Server_Obj);
      elsif Equal_Strings (Packet_Args, "c-1") then
         --  Future step/continue operations on all threads:
         Send_Gdb_Ok_Packet (Gdb_Server_Obj);
      elsif Equal_Strings (Packet_Args, "c0") then
         --  Resume target but keep attached to GDB:
         Gdb_Server_Obj.Resume_Target := True;
         Gdb_Server_Obj.Gdb_Attached := True;
         Send_Gdb_Ok_Packet (Gdb_Server_Obj);
      elsif Packet_Args (Packet_Args'First) = 'g' then
         --  Hg<thread id> packet
         declare
            Thread_Id : Integer_Address;
            Parsing_Cursor : Gdb_Packet_Data_Index_Type := Packet_Args'First + 1;
            Parsing_Ok : Boolean := False;
         begin
            Parse_Packet_Arg (Gdb_Server_Obj,
                              Packet_Args,
                              Parsing_Cursor,
                              ASCII.NUL,
                              Thread_Id,
                              Parsing_Ok);
            if not Parsing_Ok then
               return;
            end if;

            Gdb_Server_Obj.Current_Hg_Thread_Id := Thread_Id;
            Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         end;
      else
         Log_Debug_Msg_Begin ("Unsupported 'H' packet: '");
         Log_Debug_Msg_Part (Packet_Args);
         Log_Debug_Msg_End ("'");
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E5f"); --  errno 95 (ENOTSUP)
      end if;
   end Process_Uppercase_H_Packet;

   procedure Process_Lowercase_K_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
   begin
      --  TODO: to support this we need to implement the SYSTEM_RESET PSCI command.
      Send_Gdb_Error_Packet (Gdb_Server_Obj, "E5f"); --  errno 95 (ENOTSUP)
   end Process_Lowercase_K_Packet;

   procedure Send_One_Memory_Fragment (Gdb_Server_Obj : in out Gdb_Server_Type;
                                       Src_Data_Buffer : Utils.Byte_Array_Type;
                                       Is_Packet_First_Fragment : Boolean;
                                       Is_Packet_Last_Fragment : Boolean) is
      Buffer_Cursor : Gdb_Packet_Data_Index_Type := Gdb_Packet_Data_Index_Type'First;
   begin
      Gdb_Server_Obj.Gdb_Packet_Data_Length := 0;
      for Src_Byte of Src_Data_Buffer loop
         Utils.Number_Conversion.Unsigned_To_Hexadecimal_String (
            Src_Byte,
            Gdb_Server_Obj.Gdb_Packet_Data_Buffer (Buffer_Cursor .. Buffer_Cursor + 1));
         Buffer_Cursor := @ + 2;
      end loop;

      Gdb_Server_Obj.Gdb_Packet_Data_Length := Gdb_Packet_Data_Length_Type (Buffer_Cursor - 1);
      Send_Gdb_Packet_Fragment (Gdb_Server_Obj,
                                Is_Packet_First_Fragment,
                                Is_Packet_Last_Fragment);
   end Send_One_Memory_Fragment;

   procedure Parse_Packet_Arg (Gdb_Server_Obj : in out Gdb_Server_Type;
                               Packet_Args : String;
                               Parsing_Cursor : in out Gdb_Packet_Data_Index_Type;
                               Arg_Separator : Character;
                               Arg_Value : out Integer_Address;
                               Parsing_Ok : out Boolean) is
      Arg_First_Index : constant Gdb_Packet_Data_Index_Type := Parsing_Cursor;
      Arg_Last_Index : Gdb_Packet_Data_Index_Type;
   begin
      Arg_Value := Integer_Address'Last;
      if Arg_Separator = ASCII.NUL then
         Arg_Last_Index := Packet_Args'Last;
      else
         for C of Packet_Args (Parsing_Cursor .. Packet_Args'Last) loop
            exit when C = Arg_Separator;
            Parsing_Cursor := @ + 1;
         end loop;

         if Parsing_Cursor = Packet_Args'Last + 1 then
            --  No Separator found:
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
            Parsing_Ok := False;
            return;
         end if;

         Arg_Last_Index := Parsing_Cursor - 1;
         Parsing_Cursor := @ + 1; --  Skip separator
      end if;

      Utils.Number_Conversion.Hexadecimal_String_To_Unsigned (
         Packet_Args (Arg_First_Index .. Arg_Last_Index),
         Arg_Value,
         Parsing_Ok);
      if not Parsing_Ok then
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
         return;
      end if;
   end Parse_Packet_Arg;

   procedure Process_Lowercase_M_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      use CPU.Memory_Protection;
      Base_Source_Address : Integer_Address;
      Num_Source_Bytes : Integer_Address;
      Parsing_Cursor : Gdb_Packet_Data_Index_Type := Packet_Args'First;
      Parsing_Ok : Boolean := False;
   begin
      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args,
                        Parsing_Cursor,
                        ',',
                        Base_Source_Address,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      if not Valid_Readable_Data_Address (Base_Source_Address) and then
         not Valid_Code_Address (Base_Source_Address)
      then
         Log_Error_Msg ("packet 'm' invalid base address: ");
         Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (Base_Source_Address));
         Log_Error_Msg_End;
         Send_Gdb_Empty_Packet (Gdb_Server_Obj);
         return;
      end if;

      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args,
                        Parsing_Cursor,
                        ASCII.NUL,
                        Num_Source_Bytes,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      declare
         Last_Source_Address : constant Integer_Address :=
            Base_Source_Address + Num_Source_Bytes - 1;
      begin
         if not Valid_Readable_Data_Address (Last_Source_Address) and then
            not Valid_Code_Address (Last_Source_Address)
         then
            Log_Error_Msg ("packet 'm' invalid size: ");
            Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (Num_Source_Bytes));
            Log_Error_Msg_End;
            Send_Gdb_Empty_Packet (Gdb_Server_Obj);
            return;
         end if;
      end;

      declare
         Source_Data_Buffer : Utils.Byte_Array_Type (1 .. Num_Source_Bytes)
            with Import, Address => To_Address (Base_Source_Address);
         Num_Whole_Packets : constant Integer_Address :=
            (Num_Source_Bytes * 2) / Gdb_Server_Obj.Gdb_Packet_Data_Buffer'Length;
         Whole_Packet_Num_Source_Bytes : constant Integer_Address :=
            Gdb_Server_Obj.Gdb_Packet_Data_Buffer'Length / 2;
         Num_Xmit_Bytes_Last_Packet : constant Integer_Address :=
            (Num_Source_Bytes * 2) mod Gdb_Server_Obj.Gdb_Packet_Data_Buffer'Length;
         Is_Packet_First_Fragment : Boolean := True;
         Is_Packet_Last_Fragment : Boolean :=
            (if Num_Whole_Packets = 1 and then Num_Xmit_Bytes_Last_Packet = 0 then True
             else False);
         Data_Cursor : Integer_Address range Source_Data_Buffer'Range := Source_Data_Buffer'First;
      begin
         for I in 1 .. Num_Whole_Packets loop
            if I = Num_Whole_Packets and then Num_Xmit_Bytes_Last_Packet = 0 then
               Is_Packet_Last_Fragment := True;
            end if;

            Send_One_Memory_Fragment (
               Gdb_Server_Obj,
               Source_Data_Buffer (Data_Cursor .. Data_Cursor + Whole_Packet_Num_Source_Bytes - 1),
               Is_Packet_First_Fragment,
               Is_Packet_Last_Fragment);

            Data_Cursor := @ + Whole_Packet_Num_Source_Bytes;
            Is_Packet_First_Fragment := False;
         end loop;

         if Num_Xmit_Bytes_Last_Packet /= 0 then
            pragma Assert (Num_Xmit_Bytes_Last_Packet mod 2 = 0);
            Is_Packet_Last_Fragment := True;
            Send_One_Memory_Fragment (
               Gdb_Server_Obj,
               Source_Data_Buffer (Data_Cursor ..
                                   Data_Cursor + (Num_Xmit_Bytes_Last_Packet / 2) - 1),
               Is_Packet_First_Fragment,
               Is_Packet_Last_Fragment);
         end if;
      end;
   end Process_Lowercase_M_Packet;

   procedure Process_Uppercase_M_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      use CPU.Memory_Protection;
      Base_Target_Address : Integer_Address;
      Num_Target_Bytes : Integer_Address;
      Parsing_Cursor : Gdb_Packet_Data_Index_Type := Packet_Args'First;
      Parsing_Ok : Boolean := False;
   begin
      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args,
                        Parsing_Cursor,
                        ',',
                        Base_Target_Address,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      if not Valid_Writable_Data_Address (Base_Target_Address) then
         Log_Error_Msg ("packet 'M' invalid base address: ");
         Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (Base_Target_Address));
         Log_Error_Msg_End;
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
         return;
      end if;

      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args,
                        Parsing_Cursor,
                        ':',
                        Num_Target_Bytes,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      if Num_Target_Bytes = 0 then
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
         return;
      end if;

      declare
         Last_Target_Address : constant Integer_Address :=
            Base_Target_Address + Num_Target_Bytes - 1;
      begin
         if not Valid_Writable_Data_Address (Last_Target_Address) then
            Log_Error_Msg ("packet 'M' invalid size: ");
            Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (Num_Target_Bytes));
            Log_Error_Msg_End;
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
            return;
         end if;
      end;

      if (Packet_Args'Last - Parsing_Cursor + 1) mod 2 /= 0 then
         --  Odd number of hex digits:
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
         return;
      end if;

      --
      --  Convert data payload from sequence of XX hex digits to binary:
      --
      declare
         Byte_Buffer : array (1 .. Num_Target_Bytes) of Interfaces.Unsigned_8
            with Import, Address => To_Address (Base_Target_Address);
      begin
         for Byte of Byte_Buffer loop
            Utils.Number_Conversion.Hexadecimal_String_To_Unsigned (
               Packet_Args (Parsing_Cursor .. Parsing_Cursor + 1),
               Byte,
               Parsing_Ok);
            if not Parsing_Ok then
               Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
               return;
            end if;

            Parsing_Cursor := @ + 2;
         end loop;
      end;

      Send_Gdb_Ok_Packet (Gdb_Server_Obj);
   end Process_Uppercase_M_Packet;

   procedure Process_Lowercase_P_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      Gdb_Reg_Id_Value : Integer_Address;
      Parsing_Cursor : Gdb_Packet_Data_Index_Type := Packet_Args'First;
      Parsing_Ok : Boolean := False;
   begin
      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args,
                        Parsing_Cursor,
                        ASCII.NUL,
                        Gdb_Reg_Id_Value,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      if Gdb_Reg_Id_Value > Gdb_Cpu_Register_Id_Type'Pos (Gdb_Cpu_Register_Id_Type'Last) then
         Log_Error_Msg_Begin ("Invalid GDB register ID: ");
         Log_Error_Value_Hexadecimal (Interfaces.Unsigned_64 (Gdb_Reg_Id_Value));
         Log_Error_Msg_End;
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
         return;
      end if;

      declare
         use CPU.Interrupt_Handling;
         use type CPU.Cpu_Register_Type;
         Gdb_Reg_Id : constant Gdb_Cpu_Register_Id_Type :=
            Gdb_Cpu_Register_Id_Type'Enum_Val (Gdb_Reg_Id_Value);
         Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
         Interrupt_Nesting : constant Interrupt_Nesting_Type := Get_Cpu_Interrupt_Nesting (Cpu_Id);
         Stack_Pointer : constant System.Address :=
            Get_Interrupt_Nesting_Stack_Pointer (Interrupt_Nesting);
         Cpu_Context : constant Cpu_Context_Type with Import, Address => Stack_Pointer;
         --
         --  NOTE: Register values must be transmitted in target byte order,
         --  which in our case is little-endian. So, to obtain the right order
         --  of hex digit pairs, we need to convert to big endian before converting
         --  to hex digit pairs. This way, the first hex-pair transmitted is the one
         --  corresponding to the least significant byte of the original value.
         --
         Big_Endian_Reg_Value : constant CPU.Cpu_Register_Type :=
            CPU.Convert_To_Big_Endian (
               Cpu_Context.Registers (Gdb_Cpu_Register_Id_To_Cpu_Register_Id (Gdb_Reg_Id)));
      begin
         pragma Assert (Cpu_Context.Registers (SP) = CPU.Cpu_Register_Type (To_Integer (Stack_Pointer)));
         Unsigned_To_Hexadecimal_String (Interfaces.Unsigned_64 (Big_Endian_Reg_Value),
                                         Gdb_Server_Obj.Gdb_Packet_Data_Buffer);
         Gdb_Server_Obj.Gdb_Packet_Data_Length := 16;
         Send_Gdb_Packet (Gdb_Server_Obj);
      end;
   end Process_Lowercase_P_Packet;

   procedure Process_Lowercase_Q_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      Arg_End_Index : Gdb_Packet_Data_Index_Type := Packet_Args'First;
   begin
      for C of Packet_Args loop
         exit when C = ':';
         Arg_End_Index := @ + 1;
      end loop;

      Gdb_Server_Obj.Gdb_Packet_Data_Length := 0;
      declare
         use Utils;
         Arg : String renames Packet_Args (Packet_Args'First .. Arg_End_Index - 1);
         Data_Buffer : String renames Gdb_Server_Obj.Gdb_Packet_Data_Buffer;
         Data_Cursor : Gdb_Packet_Data_Index_Type := Data_Buffer'First;
         Conversion_Length : Positive;
      begin
         if Equal_Strings (Arg, "Supported") then
            Utils.Copy_String (Data_Buffer (Data_Cursor .. Data_Buffer'Last),
                               "PacketSize=",
                               Data_Cursor);
            Unsigned_To_Decimal_String (Data_Buffer'Length,
                                        Data_Buffer (Data_Cursor .. Data_Buffer'Last),
                                        Conversion_Length);
            Data_Cursor := @ + Conversion_Length;
            Utils.Copy_String (Data_Buffer (Data_Cursor .. Data_Buffer'Last),
                               ";swbreak+;hwbreak+;QNonStop-;QThreadEvents-",
                               Data_Cursor);
            Gdb_Server_Obj.Gdb_Packet_Data_Length := Gdb_Packet_Data_Length_Type (Data_Cursor - 1);
         elsif Equal_Strings (Arg, "Symbol") then
            if Arg_End_Index + 1 <= Packet_Args'Last and then
               Packet_Args (Arg_End_Index + 1) = ':'
            then
               Data_Buffer (1 .. 2) := "OK";
               Gdb_Server_Obj.Gdb_Packet_Data_Length := 2;
            else
               --  Send empty string
               null;
            end if;
         elsif Equal_Strings (Arg, "Attached") then
            Data_Buffer (1) := '1';
            Gdb_Server_Obj.Gdb_Packet_Data_Length := 1;
         elsif Equal_Strings (Arg, "C") then
            --  TODO: Get current thread Id when RTOS support is added
            null;
         elsif Equal_Strings (Arg, "fThreadInfo") then
            --  TODO: Get first thread Id when RTOS support is added
            Data_Buffer (1) := 'l';
            Gdb_Server_Obj.Gdb_Packet_Data_Length := 1;
         elsif Equal_Strings (Arg, "sThreadInfo") then
            --  TODO: Get next thread Id when RTOS support is added
            Data_Buffer (1) := 'l';
            Gdb_Server_Obj.Gdb_Packet_Data_Length := 1;
         elsif Equal_Strings (Arg, "TStatus") then
            Data_Buffer (1 .. 2) := "T0";
            Gdb_Server_Obj.Gdb_Packet_Data_Length := 2;
         end if;
      end;

      Send_Gdb_Packet (Gdb_Server_Obj);
   end Process_Lowercase_Q_Packet;

   procedure Process_Uppercase_Q_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      Arg_End_Index : Gdb_Packet_Data_Index_Type := Packet_Args'First;
   begin
      if Utils.Equal_Strings (Packet_Args, "Tinit") then
         Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         return;
      end if;

      for C of Packet_Args loop
         exit when C = ':';
         Arg_End_Index := @ + 1;
      end loop;

      declare
         use Utils;
         Arg : String renames Packet_Args (Packet_Args'First .. Arg_End_Index - 1);
      begin
         if Equal_Strings (Arg, "TDP") then
            --  TODO: Parse `QTDP:` args to set trace point
            Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         elsif Equal_Strings (Arg, "Tro") then
            --  TODO: Parse `QTro:` args
            Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         elsif Equal_Strings (Arg, "TBuffer") then
            --  TODO: Parse `QTBuffer:` args
            Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         elsif Equal_Strings (Arg, "TNotes") then
            --  TODO: Parse `QTNotes:` args
            Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         elsif Equal_Strings (Arg, "TStart") then
            --  TODO: Parse `QTStart:` args
            Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         elsif Equal_Strings (Arg, "TStop") then
            --  TODO: Parse `QTStop:` args
            Send_Gdb_Ok_Packet (Gdb_Server_Obj);
         else
            Log_Debug_Msg_Begin ("Unsupported 'Q' packet: '");
            Log_Debug_Msg_Part (Packet_Args);
            Log_Debug_Msg_End ("'");
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E5f"); --  errno 95 (ENOTSUP)
         end if;
      end;
   end Process_Uppercase_Q_Packet;

   procedure Process_Uppercase_T_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String with Unreferenced) is
   begin
      --
      --  Tell that a given thread is alive:
      --
      --  NOTE: We don't check the thread id that come sin the T packet, as
      --  we assume that all threads stay alive.
      --
      Send_Gdb_Ok_Packet (Gdb_Server_Obj);
   end Process_Uppercase_T_Packet;

   procedure Process_Lowercase_V_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      use Utils;
   begin
      if Equal_Strings (Packet_Args, "MustReplyEmpty") then
         Send_Gdb_Empty_Packet (Gdb_Server_Obj);
      elsif Equal_Strings (Packet_Args, "Kill;a") then
         --  TODO: Resetting target not supported yet (need to implement 'SYSTEM_RESET' PSCI command
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E5f"); --  errno 95 (ENOTSUP)
      elsif Equal_Strings (Packet_Args, "Cont?") then
         --  TODO: Non-stop functionality not fully supported yet. Send empty string:
         Send_Gdb_Empty_Packet (Gdb_Server_Obj);
      elsif Equal_Strings (Packet_Args, "Cont;c") then
         --  Resume target but keep attached to GDB:
         Gdb_Server_Obj.Resume_Target := True;
         Gdb_Server_Obj.Gdb_Attached := True;
      elsif Equal_Strings (Packet_Args, "Cont;s") then
         --  TODO: properly parse "Cont;s:<thread id>"
         CPU.Self_Hosted_Debug.Enable_Single_Step_Exception;
         Gdb_Server_Obj.Resume_Target := True;
         Gdb_Server_Obj.Gdb_Attached := True;
      else
         Log_Debug_Msg_Begin ("Unsupported 'v' packet: '");
         Log_Debug_Msg_Part (Packet_Args);
         Log_Debug_Msg_End ("'");
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E5f"); --  errno 95 (ENOTSUP)
      end if;
   end Process_Lowercase_V_Packet;

   procedure Process_Uppercase_X_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
      Target_Data_Addr : Integer_Address;
      Target_Data_Length : Integer_Address;
      Parsing_Cursor : Gdb_Packet_Data_Index_Type := Packet_Args'First;
      Parsing_Ok : Boolean := False;
   begin
      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args,
                        Parsing_Cursor,
                        ',',
                        Target_Data_Addr,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args,
                        Parsing_Cursor,
                        ':',
                        Target_Data_Length,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      if Target_Data_Length = 0 then
         --
         --  GDB client is checking if we support payload data in binary.
         --  We say, no, by replying with an empty packet. This way, the GDB
         --  client will fall back to use an 'M' packet.
         --
         Send_Gdb_Empty_Packet (Gdb_Server_Obj);
      else
         --  TODO: Add support for binary payload data.
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
      end if;
   end Process_Uppercase_X_Packet;

   procedure Allocate_Hardware_Breakpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                           Target_Addr : System.Address;
                                           Hardware_Breakpoint_Id : out Hardware_Breakpoint_Id_Type)
   is
      Last_Hardware_Breakpoint_Id  : constant CPU.Self_Hosted_Debug.Valid_Hardware_Breakpoint_Id_Type :=
         Gdb_Server_Obj.Hardware_Breakpoints'First +
         Valid_Hardware_Breakpoint_Id_Type (
            Gdb_Server_Obj.Self_Hosted_Debug_Capabilities.Num_Hardware_Breakpoints - 1);
   begin
      Hardware_Breakpoint_Id := Invalid_Hardware_Breakpoint_Id;
      for I in Gdb_Server_Obj.Hardware_Breakpoints'First .. Last_Hardware_Breakpoint_Id loop
         if not Gdb_Server_Obj.Hardware_Breakpoints (I).Enabled then
            Hardware_Breakpoint_Id := I;
            exit;
         end if;
      end loop;

      if Hardware_Breakpoint_Id /= Invalid_Hardware_Breakpoint_Id then
         Gdb_Server_Obj.Hardware_Breakpoints (Hardware_Breakpoint_Id) := (
            Target_Code_Addr => Target_Addr, Enabled => True);
         pragma Assert (Gdb_Server_Obj.Hardware_Breakpoints_In_Use <
                        Gdb_Server_Obj.Self_Hosted_Debug_Capabilities.Num_Hardware_Breakpoints);
         Gdb_Server_Obj.Hardware_Breakpoints_In_Use := @ + 1;
      end if;
   end Allocate_Hardware_Breakpoint;

   procedure Free_Hardware_Breakpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                       Target_Addr : System.Address;
                                       Hardware_Breakpoint_Id : out Hardware_Breakpoint_Id_Type)
   is
      Last_Hardware_Breakpoint_Id  : constant Valid_Hardware_Breakpoint_Id_Type :=
         Gdb_Server_Obj.Hardware_Breakpoints'First +
         Valid_Hardware_Breakpoint_Id_Type (
            Gdb_Server_Obj.Self_Hosted_Debug_Capabilities.Num_Hardware_Breakpoints - 1);
   begin
      Hardware_Breakpoint_Id := Invalid_Hardware_Breakpoint_Id;
      for I in Gdb_Server_Obj.Hardware_Breakpoints'First .. Last_Hardware_Breakpoint_Id loop
         if Gdb_Server_Obj.Hardware_Breakpoints (I).Target_Code_Addr = Target_Addr then
            Hardware_Breakpoint_Id := I;
            exit;
         end if;
      end loop;

      if Hardware_Breakpoint_Id /= Invalid_Hardware_Breakpoint_Id then
         Gdb_Server_Obj.Hardware_Breakpoints (Hardware_Breakpoint_Id) := (
            Target_Code_Addr => System.Null_Address, Enabled => False);
         pragma Assert (Gdb_Server_Obj.Hardware_Breakpoints_In_Use >= 1);
         Gdb_Server_Obj.Hardware_Breakpoints_In_Use := @ - 1;
      end if;
   end Free_Hardware_Breakpoint;

   procedure Allocate_Watchpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                  Target_Addr : System.Address;
                                  Watchpoint_Id : out Watchpoint_Id_Type)
   is
      Last_Watchpoint_Id  : constant Valid_Watchpoint_Id_Type :=
         Gdb_Server_Obj.Watchpoints'First +
         Valid_Watchpoint_Id_Type (
            Gdb_Server_Obj.Self_Hosted_Debug_Capabilities.Num_Watchpoints - 1);
   begin
      Watchpoint_Id := Invalid_Watchpoint_Id;
      for I in Gdb_Server_Obj.Watchpoints'First .. Last_Watchpoint_Id loop
         if not Gdb_Server_Obj.Watchpoints (I).Enabled then
            Watchpoint_Id := I;
            exit;
         end if;
      end loop;

      if Watchpoint_Id /= Invalid_Watchpoint_Id then
         Gdb_Server_Obj.Watchpoints (Watchpoint_Id) := (
            Target_Data_Addr => Target_Addr, Enabled => True);
         pragma Assert (Gdb_Server_Obj.Watchpoints_In_Use <
                        Gdb_Server_Obj.Self_Hosted_Debug_Capabilities.Num_Watchpoints);
         Gdb_Server_Obj.Watchpoints_In_Use := @ + 1;
      end if;
   end Allocate_Watchpoint;

   procedure Free_Watchpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                              Target_Addr : System.Address;
                              Watchpoint_Id : out Watchpoint_Id_Type)
   is
      Last_Watchpoint_Id  : constant Valid_Watchpoint_Id_Type :=
         Gdb_Server_Obj.Watchpoints'First +
         Valid_Watchpoint_Id_Type (
            Gdb_Server_Obj.Self_Hosted_Debug_Capabilities.Num_Watchpoints - 1);
   begin
      Watchpoint_Id := Invalid_Watchpoint_Id;
      for I in Gdb_Server_Obj.Watchpoints'First .. Last_Watchpoint_Id loop
         if Gdb_Server_Obj.Watchpoints (I).Target_Data_Addr = Target_Addr then
            Watchpoint_Id := I;
            exit;
         end if;
      end loop;

      if Watchpoint_Id /= Invalid_Watchpoint_Id then
         Gdb_Server_Obj.Watchpoints (Watchpoint_Id) := (
            Target_Data_Addr => System.Null_Address, Enabled => False);
         pragma Assert (Gdb_Server_Obj.Watchpoints_In_Use >= 1);
         Gdb_Server_Obj.Watchpoints_In_Use := @ - 1;
      end if;
   end Free_Watchpoint;

   procedure Configure_Hardware_Stop_Point (Gdb_Server_Obj : in out Gdb_Server_Type;
                                            Packet_Args : String;
                                            Enable_Stop_Point : Boolean)
   is
      procedure Configure_Hardware_Breakpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                               Target_Address : System.Address;
                                               Enable_Stop_Point : Boolean) is
         Hardware_Breakpoint_Id : Hardware_Breakpoint_Id_Type := Invalid_Hardware_Breakpoint_Id;
      begin
         --  NOTE: We don't really support software break points, but gdb uses Z0 packets
         --  for implementing stepping.
         --
         if Enable_Stop_Point then
            Allocate_Hardware_Breakpoint (Gdb_Server_Obj, Target_Address, Hardware_Breakpoint_Id);
         else
            Free_Hardware_Breakpoint (Gdb_Server_Obj, Target_Address, Hardware_Breakpoint_Id);
         end if;

         if Hardware_Breakpoint_Id = Invalid_Hardware_Breakpoint_Id then
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
            return;
         end if;

         if Enable_Stop_Point then
            CPU.Self_Hosted_Debug.Set_Hardware_Breakpoint (Hardware_Breakpoint_Id,
                                                           Target_Address);
         else
            CPU.Self_Hosted_Debug.Clear_Hardware_Breakpoint (Hardware_Breakpoint_Id);
         end if;
      end Configure_Hardware_Breakpoint;

      procedure Configure_Watchpoint (Gdb_Server_Obj : in out Gdb_Server_Type;
                                      Target_Address : System.Address;
                                      Enable_Stop_Point : Boolean) is
         Watchpoint_Id : Watchpoint_Id_Type := Invalid_Watchpoint_Id;
      begin
         --  NOTE: We don't really support software break points, but gdb uses Z0 packets
         --  for implementing stepping.
         --
         if Enable_Stop_Point then
            Allocate_Watchpoint (Gdb_Server_Obj, Target_Address, Watchpoint_Id);
         else
            Free_Watchpoint (Gdb_Server_Obj, Target_Address, Watchpoint_Id);
         end if;

         if Watchpoint_Id = Invalid_Watchpoint_Id then
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
            return;
         end if;

         if Enable_Stop_Point then
            CPU.Self_Hosted_Debug.Set_Watchpoint (Watchpoint_Id, Target_Address);
         else
            CPU.Self_Hosted_Debug.Clear_Watchpoint (Watchpoint_Id);
         end if;
      end Configure_Watchpoint;

      Cmd_Not_Supported : Boolean := False;
      Addr_Value : Integer_Address;
      Parsing_Cursor : Gdb_Packet_Data_Index_Type := Packet_Args'First;
      Parsing_Ok : Boolean := False;

   begin --  Configure_Hardware_Stop_Point
      if Packet_Args (Packet_Args'First + 1) /= ',' then
         Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
         return;
      end if;

      Parse_Packet_Arg (Gdb_Server_Obj,
                        Packet_Args (Packet_Args'First + 2 .. Packet_Args'Last),
                        Parsing_Cursor,
                        ',',
                        Addr_Value,
                        Parsing_Ok);
      if not Parsing_Ok then
         return;
      end if;

      declare
         Kind_Arg : constant Character := Packet_Args (Parsing_Cursor);
      begin
         if Kind_Arg /= '1' and then Kind_Arg /= '2' and then Kind_Arg /= '4' then
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
            return;
         end if;
      end;

      case Packet_Args (Packet_Args'First) is
         when '0' | '1' =>
            Configure_Hardware_Breakpoint (Gdb_Server_Obj, To_Address (Addr_Value),
                                           Enable_Stop_Point);
         when '2' =>
            Configure_Watchpoint (Gdb_Server_Obj, To_Address (Addr_Value),
                                  Enable_Stop_Point);
         when '3' =>
            --  Read watchpoint, not supported yet:
            Cmd_Not_Supported := True;
         when '4' =>
            --  Access watchpoint, not supported yet:
            Cmd_Not_Supported := True;
         when others =>
            Send_Gdb_Error_Packet (Gdb_Server_Obj, "E16"); --  errno 22 (EINVAL)
            return;
      end case;

      if Cmd_Not_Supported then
         Gdb_Server_Obj.Gdb_Packet_Data_Length := 0;
      else
         Gdb_Server_Obj.Gdb_Packet_Data_Buffer (1 .. 2) := "OK";
         Gdb_Server_Obj.Gdb_Packet_Data_Length := 2;
      end if;

      Send_Gdb_Packet (Gdb_Server_Obj);
   end Configure_Hardware_Stop_Point;

   procedure Process_Uppercase_Z_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
   begin
      Configure_Hardware_Stop_Point (Gdb_Server_Obj, Packet_Args,
                                     Enable_Stop_Point => True);
   end Process_Uppercase_Z_Packet;

   procedure Process_Lowercase_Z_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                         Packet_Args : String) is
   begin
      Configure_Hardware_Stop_Point (Gdb_Server_Obj, Packet_Args,
                                     Enable_Stop_Point => False);
   end Process_Lowercase_Z_Packet;

   procedure Process_Question_Mark_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                           Packet_Args : String with Unreferenced) is
   begin
      --  Reply with reason for target halted
      Send_Gdb_Stop_Reply_Packet (Gdb_Server_Obj);
   end Process_Question_Mark_Packet;

   procedure Send_Uppercase_S_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                      Signal_Num : Interfaces.Unsigned_8) is
      Data_Buffer : String renames Gdb_Server_Obj.Gdb_Packet_Data_Buffer;
   begin
      Data_Buffer (1) := 'S';
      Utils.Number_Conversion.Unsigned_To_Hexadecimal_String (
         Signal_Num, Data_Buffer (2 .. 3));
      Gdb_Server_Obj.Gdb_Packet_Data_Length := 3;
      Send_Gdb_Packet (Gdb_Server_Obj);
   end Send_Uppercase_S_Packet;

   procedure Send_Uppercase_T_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                      Signal_Num : Interfaces.Unsigned_8;
                                      Stop_Reason : String) is
      Data_Buffer : String renames Gdb_Server_Obj.Gdb_Packet_Data_Buffer;
      Data_Cursor : Gdb_Packet_Data_Index_Type := Data_Buffer'First;
   begin
       --
       --  TODO:  Add thread ID to T packet when RTOS support is added:
       --  "T<signal num><stop reason>:;thread:<thread id>;"
       --
      Data_Buffer (Data_Cursor) := 'T';
      Data_Cursor := @ + 1;
      Utils.Number_Conversion.Unsigned_To_Hexadecimal_String (
         Signal_Num, Data_Buffer (Data_Cursor .. Data_Cursor + 1));
      Data_Cursor := @ + 2;
      Data_Buffer (Data_Cursor .. Data_Cursor + Stop_Reason'Length - 1) := Stop_Reason;
      Data_Cursor := @ + Stop_Reason'Length;
      Data_Buffer (Data_Cursor .. Data_Cursor + 1) := ":;";
      Data_Cursor := @ + 2;
      Gdb_Server_Obj.Gdb_Packet_Data_Length := Gdb_Packet_Data_Length_Type (Data_Cursor - 1);
      Send_Gdb_Packet (Gdb_Server_Obj);
   end Send_Uppercase_T_Packet;

   procedure Send_Gdb_Stop_Reply_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
      Stop_Reason : constant String :=
         (case Gdb_Server_Obj.Enter_Debug_Event is
            when Hardware_Breakpoint_Event =>
               "hwbreak",
            when Watchpoint_Event =>
               "watch",
            when others =>
               ""
         );
   begin
      if Stop_Reason /= "" then
         --  Send T packet with stop reason:
         Send_Uppercase_T_Packet (Gdb_Server_Obj, Trap_Signal, Stop_Reason);
      else
         Send_Uppercase_S_Packet (Gdb_Server_Obj, Trap_Signal);
      end if;
   end Send_Gdb_Stop_Reply_Packet;

   procedure Send_Gdb_Data_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                   Data : String) is
   begin
      Gdb_Server_Obj.Gdb_Packet_Data_Buffer (1 .. Data'Length) := Data;
      Gdb_Server_Obj.Gdb_Packet_Data_Length := Data'Length;
      Send_Gdb_Packet (Gdb_Server_Obj);
   end Send_Gdb_Data_Packet;

   procedure Send_Gdb_Error_Packet (Gdb_Server_Obj : in out Gdb_Server_Type;
                                    Error_Str : String)
      renames Send_Gdb_Data_Packet;

   procedure Send_Gdb_Ok_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
   begin
      Send_Gdb_Data_Packet (Gdb_Server_Obj, "OK");
   end Send_Gdb_Ok_Packet;

   procedure Send_Gdb_Empty_Packet (Gdb_Server_Obj : in out Gdb_Server_Type) is
   begin
      --  Send empty string:
      Gdb_Server_Obj.Gdb_Packet_Data_Length := 0;
      Send_Gdb_Packet (Gdb_Server_Obj);
   end Send_Gdb_Empty_Packet;
end Gdb_Server;