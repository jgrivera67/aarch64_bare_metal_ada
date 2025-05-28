#!/bin/bash

PROJECT_DIR=$PWD

if [[ $# < 1 ]]; then
    echo "Usage: $0 <rpi4|rpi5> [--uart-boot]"
    exit 1
fi

BOARD=$1
if [[ $# = 2 && $2 = "--uart-boot" ]]; then
    UART_BOOT="yes"
    echo "*** Building embedded app uart_boot_loader_server ..."
    cd $PROJECT_DIR/embedded_apps/uart_boot_loader_server
    alr build -- -XBoard=$BOARD -XUart_Boot=no
    echo "*** Building host-side app uart_boot_loader_client ..."
    cd $PROJECT_DIR/host_apps/uart_boot_loader_client
    alr build
else
    UART_BOOT="no"
fi

for app in aarch64_hello_ada \
           aarch64_exceptions \
           aarch64_memory_protection \
           aarch64_interrupts \
           aarch64_multicore \
           aarch64_multicore_interrupts; do
   echo "*** Building embedded app $app ..."
   cd $PROJECT_DIR/embedded_apps/$app
   alr build -- -XBoard=$BOARD -XUart_Boot=$UART_BOOT
   status=$?
   if [ $status != 0 ]; then
      echo "Build of $app failed with error $status"
      exit $status
   fi
done
