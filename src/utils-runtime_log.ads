--
--  Copyright (c) 2016-2025, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary Runtime log services
--
with CPU.Multicore;
with System;

package Utils.Runtime_Log is
   use System;
   use Interfaces;

   procedure Log_Debug_Msg (Msg : String);

   procedure Log_Debug_Msg_Begin (Msg : String);

   procedure Log_Debug_Msg_Part (Msg : String);

   procedure Log_Debug_Msg_End (Msg : String := "");

   procedure Log_Debug_Value_Decimal (Value : Unsigned_32);

   procedure Log_Debug_Value_Hexadecimal (Value : Unsigned_64);

   procedure Log_Info_Msg (Msg : String);

   procedure Log_Info_Msg_Begin (Msg : String);

   procedure Log_Info_Msg_Part (Msg : String);

   procedure Log_Info_Msg_End (Msg : String := "");

   procedure Log_Info_Value_Decimal (Value : Unsigned_32);

   procedure Log_Info_Value_Hexadecimal (Value : Unsigned_64);

   procedure Log_Error_Msg (Msg : String);

   procedure Log_Error_Msg_Begin (Msg : String);

   procedure Log_Error_Msg_Part (Msg : String);

   procedure Log_Error_Msg_End (Msg : String := "");

   procedure Log_Error_Value_Decimal (Value : Unsigned_32);

   procedure Log_Error_Value_Hexadecimal (Value : Unsigned_64);

   type Runtime_Log_Level_Type is (DEBUG, INFO, ERROR, Mute);

   subtype Unmuted_Runtime_Log_Level_Type is Runtime_Log_Level_Type
      range DEBUG .. ERROR;

   --  Set the buffer logging level for the calling CPU core
   procedure Set_Buffer_Logging_Level (Log_Level : Runtime_Log_Level_Type);

   --  Get the buffer logging level for the calling CPU core
   function Get_BUffer_Logging_Level return Runtime_Log_Level_Type;

   --  Set the debug UART logging level for the calling CPU core
   procedure Set_Console_Logging_Level (Log_Level : Runtime_Log_Level_Type);

   --  Get the buffer logging level for the calling CPU core
   function Get_Console_Logging_Level return Runtime_Log_Level_Type;

   procedure Dump_Runtime_Log;

private
   use System.Storage_Elements;

   Runtime_Log_Size_In_Bytes : constant Positive := 64 * 1024; -- 64 KiB

   subtype Runtime_Log_Buffer_Index_Type is Positive range 1 .. Runtime_Log_Size_In_Bytes;

   --
   --  State variables of runtime log
   --
   type Runtime_Log_Type is limited record
      Buffer : String (Runtime_Log_Buffer_Index_Type);
      Cursor : Runtime_Log_Buffer_Index_Type := Runtime_Log_Buffer_Index_Type'First;
      Seq_Num : Interfaces.Unsigned_32 := 0;
      Wrap_Count : Interfaces.Unsigned_32 := 0;
      Buffer_Logging_Level : Runtime_Log_Level_Type := DEBUG;
      Console_Logging_Level : Runtime_Log_Level_Type := INFO;
      Unpaired_Msg_Begin_Originator : System.Address := System.Null_Address;
   end record with Alignment => CPU.Page_Size_In_Bytes;

   Runtime_Logs : array (CPU.Valid_Cpu_Core_Id_Type) of Runtime_Log_Type;

   function Get_Buffer_Logging_Level return Runtime_Log_Level_Type is
      (Runtime_Logs (CPU.Multicore.Get_Cpu_Id).Buffer_Logging_Level);

   function Get_Console_Logging_Level return Runtime_Log_Level_Type is
      (Runtime_Logs (CPU.Multicore.Get_Cpu_Id).Console_Logging_Level);

   procedure Log_Write_Msg (Runtime_Log : in out Runtime_Log_Type;
                            Log_Level : Unmuted_Runtime_Log_Level_Type;
                            Msg : String;
                            Originator_Address : System.Address;
                            Begin_Msg : Boolean := True;
                            End_Msg : Boolean := True)
      with Pre => (if Begin_Msg then Originator_Address /= System.Null_Address);

   procedure Log_Write_Msg_Value_In_Decimal (Runtime_Log : in out Runtime_Log_Type;
                                             Log_Level : Unmuted_Runtime_Log_Level_Type;
                                             Value : Unsigned_32);

   procedure Log_Write_Msg_Value_In_Hexadecimal (Runtime_Log : in out Runtime_Log_Type;
                                                 Log_Level : Unmuted_Runtime_Log_Level_Type;
                                                 Value : Unsigned_64);

   procedure Log_Write_String (Runtime_Log : in out Runtime_Log_Type;
                               Log_Level : Unmuted_Runtime_Log_Level_Type;
                               Str : String);

   procedure Log_Write_Char (Runtime_Log : in out Runtime_Log_Type;
                             Log_Level : Unmuted_Runtime_Log_Level_Type;
                             C : Character);

   procedure Dump_Log_Fragment (Runtime_Log : Runtime_Log_Type;
                                Dump_Start_Index : Runtime_Log_Buffer_Index_Type;
                                Dump_End_Index : Runtime_Log_Buffer_Index_Type);

end Utils.Runtime_Log;
