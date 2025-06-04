#!/bin/bash

PROJECT_DIR=$PWD

if [ $# != 2 ]; then
    echo "Usage: $0 <rpi4 | rpi5> <sd-boot | uart-boot>"
    exit 1
fi

BOARD=$1
if [ $2 = "uart-boot" ]; then
    UART_BOOT="yes"
    echo "*** Building embedded app uart_boot_loader_server ..."
    cd $PROJECT_DIR/embedded_apps/uart_boot_loader_server
    alr clean -- -XBoard=$BOARD -XUart_Boot=no
    alr build -- -XBoard=$BOARD -XUart_Boot=no
    status=$?
    ls -l  bin/*
    cd -
    if [ $status != 0 ]; then
        echo "Build of uart_boot_loader_server failed with error $status"
        exit $status
    fi

    echo "*** Building host-side app uart_boot_loader_client ..."
    cd $PROJECT_DIR/host_apps/uart_boot_loader_client
    alr clean
    alr build
    status=$?
    ls -l  bin/*
    cd -
    if [ $status != 0 ]; then
        echo "Build of uart_boot_loader_client failed with error $status"
        exit $status
    fi

    echo "**** Building embedded apps for $BOARD to boot from UART ****"
elif [ $2 = "sd-boot" ]; then
    UART_BOOT="no"
    echo "**** Building embedded apps for $BOARD to boot from SD card ****"
else
    echo "Usage: $0 <rpi4 | rpi5> <sd-boot | uart-boot>"
    exit 1
fi

for app in aarch64_hello_ada \
           aarch64_runtime_log \
           aarch64_exceptions \
           aarch64_memory_protection \
           aarch64_interrupts \
           aarch64_multicore \
           aarch64_multicore_interrupts; do
   echo "*** Building $app ..."
   cd $PROJECT_DIR/embedded_apps/$app
   alr clean -- -XBoard=$BOARD -XUart_Boot=$UART_BOOT
   alr build -- -XBoard=$BOARD -XUart_Boot=$UART_BOOT
   status=$?
   ls -l  bin/*
   cd -
   if [ $status != 0 ]; then
      echo "Build of $app failed with error $status"
      exit $status
   fi
done
