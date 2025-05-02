#!/bin/bash

PROJECT_DIR=$PWD

if [ #$ < 1 ]; then
    echo "Usage: $0 <rpi4|rpi5> [--uart-boot]"
    exit 1
fi

BOARD=$1
if [ $# = 2 && $2 = "--uart-boot" ]; then
    UART_BOOT="yes"
    cd $PROJECT_DIR/embedded_apps/uart_boot_loader_server
    alr build -- -XBoard=$BOARD -XUart_Boot=no
    cd $PROJECT_DIR/host_apps/uart_boot_loader_client
    alr build
else
    UART_BOOT="no"
fi

cd $PROJECT_DIR/embedded_apps/aarch64_hello_ada
alr build -- -XBoard=$BOARD -XUart_Boot=$UART_BOOT
cd $PROJECT_DIR/embedded_apps/aarch64_exceptions
alr build -- -XBoard=$BOARD -XUart_Boot=$UART_BOOT
cd $PROJECT_DIR/embedded_apps/aarch64_memory_protection
alr build -- -XBoard=$BOARD -XUart_Boot=$UART_BOOT
