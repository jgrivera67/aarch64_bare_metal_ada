.extern _stack_end

.section .text.reset_handler
.global reset_handler

reset_handler:
    mrs x0, mpidr_el1
    and x0, x0, #3 //???
    cmp x0, #0
    bne 1f

    adrp x0, _stack_end
    add x0, x0, #:lo12:_stack_end
    mov sp, x0
    bl main

1:  wfi
    b 1b
