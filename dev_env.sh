ALIRE_DIR=/opt/alire
export PATH=~/opt/cross/aarch64-elf/bin:$ALIRE_DIR/bin:~/.alire/bin:/opt/tkdiff:$PATH
export OS=macOS

export ARMFVP_DIR=/opt/FVP_Base_AEMv8R/AEMv8R_base_pkg
export ARMFVP_BIN_PATH=$ARMFVP_DIR/models/Linux64_GCC-9.3

export GIT_EXTERNAL_DIFF=tkdiff

alias v='gvim -U ide_env.vim'
alias build_rpi4_sd_boot='alr build -- -XBoard=rpi4 -XUart_Boot=no'
alias clean_rpi4_sd_boot='alr clean -- -XBoard=rpi4 -XUart_Boot=no'
alias build_rpi5_sd_boot='alr build -- -XBoard=rpi5 -XUart_Boot=no'
alias clean_rpi5_sd_boot='alr clean -- -XBoard=rpi5 -XUart_Boot=no'
alias build_rpi4_uart_boot='alr build -- -XBoard=rpi4 -XUart_Boot=yes'
alias clean_rpi4_uart_boot='alr clean -- -XBoard=rpi4 -XUart_Boot=yes'
alias build_rpi5_uart_boot='alr build -- -XBoard=rpi5 -XUart_Boot=yes'
alias clean_rpi5_uart_boot='alr clean -- -XBoard=rpi5 -XUart_Boot=yes'
alias run_ser2net='/opt/homebrew/sbin/ser2net -n -d -l -c ./ser2net.yaml -P /tmp/ser2net.pid &'
alias my_uart_rpi4='my_uart /dev/tty.usbserial-0001'
alias my_uart_rpi5='my_uart /dev/tty.usbmodem12402'

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
   picocom -b 115200 --send-cmd="$HOME/my-projects/aarch64_bare_metal_ada/host_apps/uart_boot_loader_client/bin/uart_boot_loader_client" \
                     --noreset $tty_port
}

function my_gdb
{
    typeset remote_target
    typeset elf_file

    if [ $# != 2 ]; then
            echo "Usage: $FUNCNAME <remote target> <elf file>"
            return 1
    fi

    remote_target=$1
    elf_file=$2

    #
    # NOTE: For debugging the GDB server, add the following
    # two lines before "--eval-command="target remote ..."
    # --eval-command="set debug remote 1" \
    # --eval-command="set remotetimeout 10" \
    #
    aarch64-elf-gdb \
        --eval-command="target remote $remote_target" \
        --eval-command="set output-radix 16" \
        --eval-command="set print address on" \
        --eval-command="set print array on" \
        --eval-command="set print pretty on" \
        --eval-command="set print union on" \
        --eval-command="set history save on" \
        --eval-command="set pagination off" \
        --eval-command="set max-value-size unlimited" \
        $elf_file
}

function my_gdb_rpi4 {
   typeset elf_file

   if [ $# != 1 ]; then
      echo "Usage: $FUNCNAME <elf file>"
      return 1
   fi

   elf_file=$1
   my_gdb localhost:28881 $elf_file
}

function my_gdb_rpi5 {
   typeset elf_file

   if [ $# != 1 ]; then
      echo "Usage: $FUNCNAME <elf file>"
      return 1
   fi

   elf_file=$1
   my_gdb localhost:28880 $elf_file
}

if [ -f ~/my-projects/third-party/alire/scripts/alr-completion.bash ]; then
    . ~/my-projects/third-party/alire/scripts/alr-completion.bash
fi
