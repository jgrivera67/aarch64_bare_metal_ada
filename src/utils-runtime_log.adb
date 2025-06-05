--
--  Copyright (c) 2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

with CPU;
with CPU.Interrupt_Handling;
with Gdb_Server;
with Timer_Driver;
with Utils.Number_Conversion;

package body Utils.Runtime_Log is
   procedure Log_Debug_Msg (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
      Originator_Address : constant System.Address := CPU.Get_Call_Address;
   begin
      Log_Write_Msg (Runtime_Log, DEBUG, Msg, Originator_Address);
   end Log_Debug_Msg;

   procedure Log_Debug_Msg_Begin (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
      Originator_Address : constant System.Address := CPU.Get_Call_Address;
   begin
      Log_Write_Msg (Runtime_Log, DEBUG, Msg, Originator_Address,
                     Begin_Msg => True, End_Msg => False);
   end Log_Debug_Msg_Begin;

   procedure Log_Debug_Msg_Part (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg (Runtime_Log, DEBUG, Msg, System.Null_Address,
                     Begin_Msg => False, End_Msg => False);
   end Log_Debug_Msg_Part;

   procedure Log_Debug_Msg_End (Msg : String := "") is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg (Runtime_Log, DEBUG, Msg, System.Null_Address,
                     Begin_Msg => False, End_Msg => True);
   end Log_Debug_Msg_End;

   procedure Log_Debug_Value_Decimal (Value : Unsigned_32) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg_Value_In_Decimal (Runtime_Log, DEBUG, Value);
   end Log_Debug_Value_Decimal;

   procedure Log_Debug_Value_Hexadecimal (Value : Unsigned_64) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg_Value_In_Hexadecimal (Runtime_Log, DEBUG, Value);
   end Log_Debug_Value_Hexadecimal;

   procedure Log_Info_Msg (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
      Originator_Address : constant System.Address := CPU.Get_Call_Address;
   begin
      Log_Write_Msg (Runtime_Log, INFO, Msg, Originator_Address);
   end Log_Info_Msg;

   procedure Log_Info_Msg_Begin (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
      Originator_Address : constant System.Address := CPU.Get_Call_Address;
   begin
      Log_Write_Msg (Runtime_Log, INFO, Msg, Originator_Address,
                     Begin_Msg => True, End_Msg => False);
   end Log_Info_Msg_Begin;

   procedure Log_Info_Msg_Part (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg (Runtime_Log, INFO, Msg, System.Null_Address,
                     Begin_Msg => False, End_Msg => False);
   end Log_Info_Msg_Part;

   procedure Log_Info_Msg_End (Msg : String := "") is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg (Runtime_Log, INFO, Msg, System.Null_Address,
                     Begin_Msg => False, End_Msg => True);
   end Log_Info_Msg_End;

   procedure Log_Info_Value_Decimal (Value : Unsigned_32) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg_Value_In_Decimal (Runtime_Log, INFO, Value);
   end Log_Info_Value_Decimal;

   procedure Log_Info_Value_Hexadecimal (Value : Unsigned_64) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg_Value_In_Hexadecimal (Runtime_Log, INFO, Value);
   end Log_Info_Value_Hexadecimal;

   procedure Log_Error_Msg (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
      Originator_Address : constant System.Address := CPU.Get_Call_Address;
   begin
      Log_Write_Msg (Runtime_Log, ERROR, Msg, Originator_Address);
   end Log_Error_Msg;

   procedure Log_Error_Msg_Begin (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
      Originator_Address : constant System.Address := CPU.Get_Call_Address;
   begin
      Log_Write_Msg (Runtime_Log, ERROR, Msg, Originator_Address,
                     Begin_Msg => True, End_Msg => False);
   end Log_Error_Msg_Begin;

   procedure Log_Error_Msg_Part (Msg : String) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg (Runtime_Log, ERROR, Msg, System.Null_Address,
                     Begin_Msg => False, End_Msg => False);
   end Log_Error_Msg_Part;

   procedure Log_Error_Msg_End (Msg : String := "") is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg (Runtime_Log, ERROR, Msg, System.Null_Address,
                     Begin_Msg => False, End_Msg => True);
   end Log_Error_Msg_End;

   procedure Log_Error_Value_Decimal (Value : Unsigned_32) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg_Value_In_Decimal (Runtime_Log, ERROR, Value);
   end Log_Error_Value_Decimal;

   procedure Log_Error_Value_Hexadecimal (Value : Unsigned_64) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Log_Write_Msg_Value_In_Hexadecimal (Runtime_Log, ERROR, Value);
   end Log_Error_Value_Hexadecimal;

   procedure Set_Buffer_Logging_Level (Log_Level : Runtime_Log_Level_Type) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Runtime_Log.Buffer_Logging_Level := Log_Level;
   end Set_Buffer_Logging_Level;

   procedure Set_Console_Logging_Level (Log_Level : Runtime_Log_Level_Type) is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
   begin
      Runtime_Log.Console_Logging_Level := Log_Level;
   end Set_Console_Logging_Level;

   procedure Dump_Runtime_Log is
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      Runtime_Log : Runtime_Log_Type renames Runtime_Logs (Cpu_Id);
      Dump_End_Index : Runtime_Log_Buffer_Index_Type;
      Dump_Start_Index : Runtime_Log_Buffer_Index_Type;
   begin
      Dump_End_Index := Runtime_Log.Cursor;
      if Runtime_Log.Wrap_Count = 0 then
         Dump_Start_Index := Runtime_Log.Buffer'First;
      else
         Dump_Start_Index := Dump_End_Index;
      end if;

      Utils.Lock_Console (Print_Cpu => False);
      Utils.Print_String (ASCII.LF & "*** Dumping Runtime log for CPU ");
      Utils.Print_Number_Decimal (Unsigned_32 (Cpu_Id));
      Utils.Print_String (": log wrap count: ");
      Utils.Print_Number_Decimal (Runtime_Log.Wrap_Count);
      Utils.Print_String (" ***" & ASCII.LF);
      Utils.Print_String (
         "(cpu:sequence number:time stamp:log level:origination address:message)" &
          ASCII.LF);
      Dump_Log_Fragment (Runtime_Log, Dump_Start_Index, Dump_End_Index);
      Utils.Print_String ("*** End of Runtime Log Dump ***" & ASCII.LF);
      Utils.Unlock_Console;
   end Dump_Runtime_Log;

   procedure Log_Write_Msg (Runtime_Log : in out Runtime_Log_Type;
                            Log_Level : Unmuted_Runtime_Log_Level_Type;
                            Msg : String;
                            Originator_Address : System.Address;
                            Begin_Msg : Boolean := True;
                            End_Msg : Boolean := True)
   is
      use Timer_Driver;
   begin
      if Log_Level < Runtime_Log.Buffer_Logging_Level then
         return;
      end if;

      declare
         Old_Cpu_Interrupting_State : constant CPU.Cpu_Register_Type :=
            CPU.Interrupt_Handling.Disable_Cpu_Interrupting;
         Timestamp_Us : constant Timestamp_Us_Type := Get_Timestamp_Us;
         Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
      begin
         if Begin_Msg then
            declare
               use Utils.Number_Conversion;
               Buffer : Unsigned_64_Hexadecimal_String_Type;
               Num_Digits : Positive;
               Short_Timestamp : constant Unsigned_32 :=
                  Unsigned_32 (Timestamp_Us and Timestamp_Us_Type (Unsigned_32'Last));
            begin
               pragma Assert (Runtime_Log.Unpaired_Msg_Begin_Originator = System.Null_Address);
               Runtime_Log.Unpaired_Msg_Begin_Originator := Originator_Address;
               if Log_Level >= Runtime_Log.Console_Logging_Level then
                  Utils.Lock_Console (Print_Cpu => False);
               end if;

               Log_Write_String (Runtime_Log, Log_Level, "CPU");
               Unsigned_To_Decimal_String (Unsigned_32 (Cpu_Id), Buffer, Num_Digits);
               Log_Write_String (Runtime_Log, Log_Level, Buffer (1 .. Num_Digits));
               Log_Write_Char (Runtime_Log, Log_Level, ':');
               Unsigned_To_Decimal_String (Runtime_Log.Seq_Num, Buffer, Num_Digits);
               Log_Write_String (Runtime_Log, Log_Level, Buffer (1 .. Num_Digits));
               Log_Write_Char (Runtime_Log, Log_Level, ':');
               Unsigned_To_Decimal_String (Short_Timestamp, Buffer, Num_Digits);
               Log_Write_String (Runtime_Log, Log_Level, Buffer (1 .. Num_Digits));
               Log_Write_Char (Runtime_Log, Log_Level, ':');
               Log_Write_String (Runtime_Log, Log_Level, Log_Level'Image);
               Log_Write_Char (Runtime_Log, Log_Level, ':');
               Unsigned_To_Hexadecimal_String (
                  Unsigned_64 (To_Integer (Originator_Address)), Buffer);
               Log_Write_String (Runtime_Log, Log_Level, Buffer);
               Log_Write_Char (Runtime_Log, Log_Level, ':');
            end;
         end if;

         Log_Write_String (Runtime_Log, Log_Level, Msg);

         if End_Msg then
            pragma Assert (Runtime_Log.Unpaired_Msg_Begin_Originator /= System.Null_Address);
            Runtime_Log.Unpaired_Msg_Begin_Originator := System.Null_Address;
            Log_Write_Char (Runtime_Log, Log_Level, ASCII.LF);
            Runtime_Log.Seq_Num := Runtime_Log.Seq_Num + 1;
            if Log_Level >= Runtime_Log.Console_Logging_Level then
               Utils.Unlock_Console;
            end if;
         end if;

         CPU.Interrupt_Handling.Restore_Cpu_Interrupting (Old_Cpu_Interrupting_State);
      end;
   end Log_Write_Msg;

   procedure Log_Write_Msg_Value_In_Decimal (Runtime_Log : in out Runtime_Log_Type;
                                             Log_Level : Unmuted_Runtime_Log_Level_Type;
                                             Value : Unsigned_32)
   is
      Buffer : Utils.Number_Conversion.Unsigned_32_Decimal_String_Type;
      Num_Digits : Positive;
   begin
      Utils.Number_Conversion.Unsigned_To_Decimal_String (Value, Buffer, Num_Digits);
      Log_Write_Msg (Runtime_Log,
                     Log_Level,
                     Buffer (1 .. Num_Digits),
                     System.Null_Address,
                     Begin_Msg => False,
                     End_Msg => False);
   end Log_Write_Msg_Value_In_Decimal;

   procedure Log_Write_Msg_Value_In_Hexadecimal (Runtime_Log : in out Runtime_Log_Type;
                                                 Log_Level : Unmuted_Runtime_Log_Level_Type;
                                                 Value : Unsigned_64)
   is
      Buffer : Utils.Number_Conversion.Unsigned_64_Hexadecimal_String_Type;
   begin
      Utils.Number_Conversion.Unsigned_To_Hexadecimal_String (Value, Buffer);
      Log_Write_Msg (Runtime_Log,
                     Log_Level,
                     Buffer,
                     System.Null_Address,
                     Begin_Msg => False,
                     End_Msg => False);
   end Log_Write_Msg_Value_In_Hexadecimal;

   procedure Log_Write_String (Runtime_Log : in out Runtime_Log_Type;
                               Log_Level : Unmuted_Runtime_Log_Level_Type;
                               Str : String) is
   begin
      for C of Str loop
         Log_Write_Char (Runtime_Log, Log_Level, C);
      end loop;
   end Log_Write_String;

   procedure Log_Write_Char (Runtime_Log : in out Runtime_Log_Type;
                             Log_Level : Unmuted_Runtime_Log_Level_Type;
                             C : Character) is
      use type CPU.Valid_Cpu_Core_Id_Type;
      Cursor : Runtime_Log_Buffer_Index_Type;
      Cpu_Id : constant CPU.Valid_Cpu_Core_Id_Type := CPU.Multicore.Get_Cpu_Id;
   begin
      Cursor := Runtime_Log.Cursor;
      Runtime_Log.Buffer (Cursor) := C;
      if Cursor = Runtime_Log.Buffer'Last then
         Cursor := Runtime_Log.Buffer'First;
         Runtime_Log.Wrap_Count := Runtime_Log.Wrap_Count + 1;
      else
         Cursor := Cursor + 1;
      end if;

      Runtime_Log.Cursor := Cursor;

      --
      --  Print character to UART:
      --
      if Log_Level >= Runtime_Log.Console_Logging_Level and then
         (CPU.Cpu_Is_Multicore_Synchronization_Ready or else
          Cpu_Id = CPU.Valid_Cpu_Core_Id_Type'First) and then
         not Gdb_Server.Gdb_Server_Is_Running
      then
         if Utils.Console_Lock_Owner = Cpu_Id or else
            not CPU.Cpu_Is_Multicore_Synchronization_Ready
         then
            Utils.Put_Char (C);
         else
            Utils.Print_String (
               "*** WARNING: Cannot log messages to the console because console lock is not owned by CPU ");
            Utils.Print_Number_Decimal (Unsigned_32 (Cpu_Id), End_Line => True);
         end if;
      end if;
   end Log_Write_Char;

   procedure Dump_Log_Fragment (Runtime_Log : Runtime_Log_Type;
                                Dump_Start_Index : Runtime_Log_Buffer_Index_Type;
                                Dump_End_Index : Runtime_Log_Buffer_Index_Type)
   is
      Dump_Cursor : Runtime_Log_Buffer_Index_Type := Dump_Start_Index;
      Char_Value : Character;
   begin
      if Dump_Start_Index = Dump_End_Index and then Runtime_Log.Wrap_Count = 0 then
         --  Log buffer empty
         return;
      end if;

      loop
         Char_Value := Runtime_Log.Buffer (Dump_Cursor);
         Utils.Put_Char (Char_Value);
         if Dump_Cursor = Runtime_Log.Buffer'Last then
            Dump_Cursor := Runtime_Log.Buffer'First;
         else
            Dump_Cursor := Dump_Cursor + 1;
         end if;

         exit when Dump_Cursor = Dump_End_Index;
      end loop;
   end Dump_Log_Fragment;
end Utils.Runtime_Log;
