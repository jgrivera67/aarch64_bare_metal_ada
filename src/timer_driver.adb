--
--  Copyright (c) 2022-2023, German Rivera
--
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  @summary ARM generic timer driver
--

with System.Machine_Code;

package body Timer_Driver is
   procedure Initialize is
      CNTP_CTL_Value : CNTP_CTL_Type;
   begin
      CNTP_CTL_Value := Get_CNTV_CTL;
      CNTP_CTL_Value.ENABLE := Timer_Disabled;
      CNTP_CTL_Value.IMASK := Timer_Interrupt_Masked;
      Set_CNTV_CTL (CNTP_CTL_Value);

      --
      --  NOTE: the generic timer interrupt is enabled in the GIC and in the
      --  peripheral when Start_Timer is called and disabled when Stop_Timer
      --  is called.
      --
   end Initialize;

   function Get_CNTP_CTL return CNTP_CTL_Type is
      CNTP_CTL_Value : CNTP_CTL_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntp_ctl_el0",
         Outputs => CNTP_CTL_Type'Asm_Output ("=r", CNTP_CTL_Value), --  %0
         Volatile => True);

      return CNTP_CTL_Value;
   end Get_CNTP_CTL;

   procedure Set_CNTP_CTL (CNTP_CTL_Value : CNTP_CTL_Type) is
   begin
      System.Machine_Code.Asm (
         "msr cntp_ctl_el0, %0",
         Inputs => CNTP_CTL_Type'Asm_Input ("r", CNTP_CTL_Value), --  %0
         Volatile => True);
   end Set_CNTP_CTL;

   function Get_CNTV_CTL return CNTP_CTL_Type is
      CNTP_CTL_Value : CNTP_CTL_Type;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntv_ctl_el0",
         Outputs => CNTP_CTL_Type'Asm_Output ("=r", CNTP_CTL_Value), --  %0
         Volatile => True);

      return CNTP_CTL_Value;
   end Get_CNTV_CTL;

   procedure Set_CNTV_CTL (CNTP_CTL_Value : CNTP_CTL_Type) is
   begin
      System.Machine_Code.Asm (
         "msr cntv_ctl_el0, %0",
         Inputs => CNTP_CTL_Type'Asm_Input ("r", CNTP_CTL_Value), --  %0
         Volatile => True);
   end Set_CNTV_CTL;

   function Get_CNTP_TVAL return Interfaces.Unsigned_64 is
      CNTP_TVAL_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntp_tval_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTP_TVAL_Value), --  %0
         Volatile => True);

      return CNTP_TVAL_Value;
   end Get_CNTP_TVAL;

   procedure Set_CNTP_TVAL (CNTP_TVAL_Value : Interfaces.Unsigned_64) is
   begin
      System.Machine_Code.Asm (
         "msr cntp_tval_el0, %0",
         Inputs => Interfaces.Unsigned_64'Asm_Input ("r", CNTP_TVAL_Value), --  %0
         Volatile => True);
   end Set_CNTP_TVAL;

   function Get_CNTPCT return Interfaces.Unsigned_64 is
      CNTPCT_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntpct_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTPCT_Value), --  %0
         Volatile => True);

      return CNTPCT_Value;
   end Get_CNTPCT;

   function Get_CNTVCT return Interfaces.Unsigned_64 is
      CNTVCT_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntvct_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTVCT_Value), --  %0
         Volatile => True);

      return CNTVCT_Value;
   end Get_CNTVCT;

   function Get_CNTFRQ return Interfaces.Unsigned_64 is
      CNTFRQ_Value : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm (
         "mrs %0, cntfrq_el0",
         Outputs => Interfaces.Unsigned_64'Asm_Output ("=r", CNTFRQ_Value), --  %0
         Volatile => True);

      return CNTFRQ_Value;
   end Get_CNTFRQ;

end Timer_Driver;