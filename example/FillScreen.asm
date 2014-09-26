        .org $FF00

        LDX #$10

loop:
        LDA #$01
        STA $0200,X

        LDA #$05
        STA $0201,X

        LDA #$08
        STA $0202,X

        INX
        INX
        INX

        JMP loop
