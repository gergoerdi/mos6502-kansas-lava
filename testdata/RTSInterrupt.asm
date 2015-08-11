	.org $f000

reset:  lda #$42
        sta $1234
        jsr sub
        jmp *

sub:
        cli
        rts

irq:
        lda #$EA                ; NOTE: $EA == NOP so if entering the interrupt  is wrong, it is still "safe"
        sta $1234
        rti


        .res $FFFC-*

        .org $FFFC
resetv: .addr reset
irqv:   .addr irq
