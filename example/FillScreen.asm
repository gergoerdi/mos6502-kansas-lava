        .org $F000

        LDX #$10

loop:
        LDA #$01
        STA $0200,X
        INX

        LDA #$05
        STA $0200,X
        INX

        LDA #$08
        STA $0200,X
        INX

        JMP loop
