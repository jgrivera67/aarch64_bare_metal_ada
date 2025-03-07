pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__app_main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__app_main.adb");
pragma Suppress (Overflow_Check);

package body ada_main is

   E21 : Short_Integer; pragma Import (Ada, E21, "uart_driver_E");
   E04 : Short_Integer; pragma Import (Ada, E04, "uart_boot_loader_E");
   E12 : Short_Integer; pragma Import (Ada, E12, "uart_boot_loader__xmodem_protocol_E");


   procedure adainit is
   begin
      null;

      E21 := E21 + 1;
      E12 := E12 + 1;
      E04 := E04 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_app_main");

   procedure main is
      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      adainit;
      Ada_Main_Program;
   end;

--  BEGIN Object file/option list
   --   /Users/jgrivera/my-projects/HiRTOS/sample_apps/uart_boot_loader/obj/development/bit_sized_integer_types.o
   --   /Users/jgrivera/my-projects/HiRTOS/sample_apps/uart_boot_loader/obj/development/uart_driver.o
   --   /Users/jgrivera/my-projects/HiRTOS/sample_apps/uart_boot_loader/obj/development/uart_boot_loader-xmodem_protocol.o
   --   /Users/jgrivera/my-projects/HiRTOS/sample_apps/uart_boot_loader/obj/development/uart_boot_loader.o
   --   /Users/jgrivera/my-projects/HiRTOS/sample_apps/uart_boot_loader/obj/development/app_main.o
   --   -L/Users/jgrivera/my-projects/HiRTOS/sample_apps/uart_boot_loader/obj/development/
   --   -L/Users/jgrivera/my-projects/HiRTOS/sample_apps/uart_boot_loader/obj/development/
   --   -L/Users/jgrivera/my-projects/HiRTOS/third_party/portable_minimal_ada_rts/lib/
   --   -L/Users/jgrivera/my-projects/HiRTOS/third_party/portable_minimal_ada_rts/obj/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
