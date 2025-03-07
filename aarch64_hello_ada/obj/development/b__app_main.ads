pragma Warnings (Off);
pragma Ada_95;
pragma Restrictions (No_Exception_Propagation);
with System;
package ada_main is


   GNAT_Version : constant String :=
                    "GNAT Version: 14.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_app_main" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure main;
   pragma Export (C, main, "main");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  ada.exceptions%s
   --  ada.exceptions%b
   --  ada.assertions%s
   --  ada.assertions%b
   --  gnat%s
   --  gnat.source_info%s
   --  system.machine_code%s
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.assertions%s
   --  system.assertions%b
   --  bit_sized_integer_types%s
   --  uart_driver%s
   --  uart_driver%b
   --  uart_boot_loader%s
   --  uart_boot_loader.xmodem_protocol%s
   --  uart_boot_loader.xmodem_protocol%b
   --  uart_boot_loader%b
   --  app_main%b
   --  END ELABORATION ORDER

end ada_main;
