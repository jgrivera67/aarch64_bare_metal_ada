ALIRE_DIR=/opt/alire
export PATH=~/opt/cross/aarch64-elf/bin:$ALIRE_DIR/bin:~/.alire/bin:/opt/tkdiff:$PATH
export OS=macOS

export ARMFVP_DIR=/opt/FVP_Base_AEMv8R/AEMv8R_base_pkg
export ARMFVP_BIN_PATH=$ARMFVP_DIR/models/Linux64_GCC-9.3

export GIT_EXTERNAL_DIFF=tkdiff

alias v='gvim -U ide_env.vim'
alias build_for_uart_boot='alr build -- -XUart_Boot=yes'

function run_fvp {
   typeset elf_file

   if [ $# != 1 ]; then
        echo "Usage: $FUNCNAME <elf file>"
        return 1
   fi

   elf_file=$1

   #
   # NOTE:
   # - `cluster0.gicv3.SRE-EL2-enable-RAO=1` and `cluster0.gicv3.cpuintf-mmap-access-level=2`
   #   are needed to enable access to the GIC CPU interface's system registers
   # - `bp.refcounter.non_arch_start_at_default=1` enables the system counter that drives
   #   the generic timer counter.
   #
   $ARMFVP_BIN_PATH/FVP_BaseR_AEMv8R \
           -C bp.pl011_uart0.uart_enable=1 \
           -C bp.pl011_uart0.baud_rate=460800 \
           -C bp.pl011_uart1.uart_enable=1 \
           -C bp.pl011_uart1.baud_rate=460800 \
           -C bp.pl011_uart2.uart_enable=1 \
           -C bp.pl011_uart2.baud_rate=460800 \
           -C bp.pl011_uart3.uart_enable=1 \
           -C bp.pl011_uart3.baud_rate=460800 \
           -C cluster0.gicv3.SRE-EL2-enable-RAO=1 \
           -C cluster0.gicv3.cpuintf-mmap-access-level=2 \
           -C bp.refcounter.non_arch_start_at_default=1 \
	   --application $elf_file #--log ~/tmp/fvp-run.log

	   #-C cci400.force_on_from_start=1 \
           #-C bp.sram.enable_atomic_ops=1 \
           #-C cci400.force_on_from_start=1 \
           #-C cluster0.gicv3.EOI-check-ID=1 \
           #-C cluster0.gicv3.EOI-check-CPUID=1 \
}

function run_fvp_with_trace {
   typeset elf_file

   if [ $# != 1 ]; then
        echo "Usage: $FUNCNAME <elf file>"
        return 1
   fi

   elf_file=$1
   $ARMFVP_BIN_PATH/FVP_BaseR_AEMv8R \
	   --plugin=$ARMFVP_DIR//plugins/Linux64_GCC-9.3/TarmacTrace.so \
	   --parameter TRACE.TarmacTrace.trace-file="STDERR" \
           -C bp.pl011_uart0.uart_enable=1 \
           -C bp.pl011_uart0.baud_rate=115200 \
           -C cluster0.gicv3.SRE-EL2-enable-RAO=1 \
           -C cluster0.gicv3.cpuintf-mmap-access-level=2 \
           -C bp.refcounter.non_arch_start_at_default=1 \
	   -C cci400.force_on_from_start=1 \
	   --application $elf_file # 2> ~/tmp/fvp-trace.pipe
}

function gen_lst_arm64
{
    typeset elf_file
    typeset usage_msg

    usage_msg="Usage: gen_lst <elf file>"

    if [ $# != 1 ]; then
        echo $usage_msg
        return 1
    fi

    elf_file=$1
    if [ ! -f $elf_file ]; then
        echo "*** ERROR: file $elf_file does not exist"
        return 1
    fi

    rm -f $elf_file.lst

    echo "Generating $elf_file.lst ..."
    aarch64-elf-objdump -dSstl $elf_file > $elf_file.lst

    if [ ! -f $elf_file.lst ]; then
        echo "*** ERROR: file $elf_file.lst not created"
        return 1;
    fi
}

function flash_raspberry4
{
    typeset bin_file
    typeset tty_dev
    typeset usage_msg

    usage_msg="Usage: $FUNCNAME <bin file>"

    if [ $# != 1 ]; then
        echo $usage_msg
        return 1
    fi

    bin_file=$1
    if [ ! -f $bin_file ]; then
        echo "*** ERROR: file $bin_file does not exist"
        return 1
    fi

   # Flash App image on SD card:
   cp $bin_file /Volumes/bootfs/kernel8.img
   sync
}

function send_bin_over_uart {
   typeset bin_file
   typeset tty_port

   if [ $# != 2 ]; then
        echo "Usage: $FUNCNAME <bin file> <tty port>"
        return 1
   fi

   bin_file=$1
   tty_port=$2
   stty -f $tty_port 115200  #configure to the baud rate of the embedded system
   #lsx --xmodem --binary $bin_file > $tty_port < $tty_port
   ~/my-projects/aarch64_bare_metal_ada/uart_boot_loader_client/bin/uart_boot_loader_client $bin_file > $tty_port < $tty_port
}

function my_uart {
   typeset tty_port

   if [ $# != 1 ]; then
        echo "Usage: $FUNCNAME <tty port>"
        return 1
   fi

   tty_port=$1

   #picocom -b 115200 --send-cmd="lsx -vv --xmodem --binary" --receive-cmd="lrx -vv" $tty_port
   picocom -b 115200 --send-cmd="$HOME/my-projects/aarch64_bare_metal_ada/uart_boot_loader_client/bin/uart_boot_loader_client" $tty_port
}

function my_gdb
{
    typeset tty_name
    typeset elf_file

    if [ $# != 2 ]; then
            echo "Usage: $FUNCNAME <tty name> <elf file>"
            return 1
    fi

    tty_name=$1
    elf_file=$2

    arm-none-eabi-gdb -b 115200 \
        --eval-command="target remote $tty_name" \
        --eval-command="set output-radix 16" \
        --eval-command="set print address on" \
        --eval-command="set print array on" \
        --eval-command="set print pretty on" \
        --eval-command="set print union on" \
        --eval-command="set history save on" \
        --eval-command="set pagination off" \
        $elf_file

        #--eval-command="set debug remote 1" \
}


. ~/my-projects/third-party/alire/scripts/alr-completion.bash
