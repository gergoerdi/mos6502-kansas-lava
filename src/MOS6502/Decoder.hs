{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module MOS6502.Decoder
       ( AddrMode(..), AddrOffset(..)
       , ArgReg(..), BranchFlag(..)
       , JumpCall(..), PushPop(..), StackArg(..)
       , OpClass(..), opALU, opChangeFlag, opBranch, opPush, opPop
       , ALUOp(..), aluBinOp, aluUnOp
       , Decoded(..), decode
       , decode', unpackDecoded
       ) where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU

import Language.Literals.Binary
import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Tuple (swap)

data AddrMode = AddrNone
              | AddrImm
              | AddrZP
              | AddrIndirect
              | AddrDirect
              deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''AddrMode 3); instance BitRep AddrMode where bitRep = bitRepEnum

data AddrOffset = OffsetNone
                | OffsetPreAddX
                | OffsetPreAddY
                | OffsetPostAddY
              deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''AddrOffset 2); instance BitRep AddrOffset where bitRep = bitRepEnum

data Addressing clk = Addressing{ addrNone :: Signal clk Bool
                                , addrImm :: Signal clk Bool
                                , addrZP :: Signal clk Bool
                                , addrPreAddX :: Signal clk Bool
                                , addrPreAddY :: Signal clk Bool
                                , addrPostAddY :: Signal clk Bool
                                , addrIndirect :: Signal clk Bool
                                , addrDirect :: Signal clk Bool
                                }
                    deriving Show

data BranchFlag = BranchN
                | BranchV
                | BranchC
                | BranchZ
                deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''BranchFlag 2); instance BitRep BranchFlag where bitRep = bitRepEnum

data ArgReg = RegA
            | RegX
            | RegY
            | RegSP
            deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''ArgReg 2); instance BitRep ArgReg where bitRep = bitRepEnum

data ALUOp = ALUBin BinOp
           | ALUUn UnOp
           | ALUCmp
           | ALUBIT
           deriving (Eq, Ord, Show)
$(repBitRep ''ALUOp 5)

instance BitRep ALUOp where
    bitRep = concat [ [(ALUBin, bits "00")] &* bitRep
                    , [(ALUUn, bits "01")] &* bitRep
                    , [(ALUCmp, bits "00010")]
                    , [(ALUBIT, bits "00011")]
                    ]

aluBinOp :: forall clk. Signal clk ALUOp -> Signal clk (Enabled BinOp)
aluBinOp s = muxN [ (sel .==. [b|00|], enabledS bin)
                  , (high, disabledS)
                  ]
  where
    (sel :: Signal clk X4, bin) = unappendS $ s

aluUnOp :: forall clk. Signal clk ALUOp -> Signal clk (Enabled UnOp)
aluUnOp s = muxN [ (sel .==. [b|01|], enabledS un)
                 , (high, disabledS)
                 ]
  where
    (sel :: Signal clk X4, un) = unappendS $ s

data JumpCall = Jump | Call
              deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''JumpCall 1); instance BitRep JumpCall where bitRep = bitRepEnum

data PushPop = Push | Pop
              deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''PushPop 1); instance BitRep PushPop where bitRep = bitRepEnum

data StackArg = StackArgA | StackArgP
              deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''StackArg 1); instance BitRep StackArg where bitRep = bitRepEnum

data OpClass = OpALU ALUOp
             | OpBranch BranchFlag Bool
             | OpFlag X8 Bool
             | OpJumpCall JumpCall
             | OpPushPop PushPop StackArg
             | OpRTS
             | OpRTI
             | OpBRK
             deriving (Eq, Ord, Show)
$(repBitRep ''OpClass 8)

instance BitRep OpClass where
    bitRep = concat [ [(OpALU,      bits (           "000"))] &* bitRep
                    , [(OpBranch,   bits ("00"    ++ "001"))] &* bitRep     &* bitRepEnum
                    , [(OpFlag,     bits ("0"     ++ "010"))] &* bitRepEnum &* bitRepEnum
                    , [(OpJumpCall, bits ("0000"  ++ "011"))] &* bitRep
                    , [(OpPushPop,  bits ("000"   ++ "100"))] &* bitRep     &* bitRep
                    , [(OpRTS,      bits ("00000" ++ "101"))]
                    , [(OpRTI,      bits ("00000" ++ "110"))]
                    , [(OpBRK,      bits ("00000" ++ "111"))]
                    ]

opALU :: forall clk. Signal clk OpClass -> Signal clk (Enabled ALUOp)
opALU op = packEnabled (sel .==. 0x0) val
  where
    (sel :: Signal clk U3, val) = unappendS op

opBranch :: forall clk. Signal clk OpClass -> Signal clk (Enabled (BranchFlag, Bool))
opBranch op = packEnabled (sel .==. 0x1) val
  where
    (sel :: Signal clk U5, val) = unappendS op

opChangeFlag :: forall clk. Signal clk OpClass -> Signal clk (Enabled (X8, Bool))
opChangeFlag op = packEnabled (sel .==. 0x2) val
  where
    (sel :: Signal clk U4, val) = unappendS op

opPush :: forall clk. Signal clk OpClass -> Signal clk (Enabled StackArg)
opPush op = packEnabled (sel .==. 0x4 .&&. dir .==. pureS Push) arg
  where
    (sel :: Signal clk U6, val :: Signal clk (PushPop, StackArg)) = unappendS op
    (dir, arg) = unpack val

opPop :: forall clk. Signal clk OpClass -> Signal clk (Enabled StackArg)
opPop op = packEnabled (sel .==. 0x4 .&&. dir .==. pureS Pop) arg
  where
    (sel :: Signal clk U6, val :: Signal clk (PushPop, StackArg)) = unappendS op
    (dir, arg) = unpack val

data Decoded clk = Decoded{ dAddrMode :: Signal clk AddrMode
                          , dAddrOffset :: Signal clk AddrOffset
                          , dSourceReg :: Signal clk (Enabled ArgReg)
                          , dReadMem :: Signal clk Bool
                          , dTargetReg :: Signal clk (Enabled ArgReg)
                          , dWriteMem :: Signal clk Bool
                          , dOp :: Signal clk OpClass
                          , dUpdateFlags :: Signal clk Bool
                          }
                 deriving Show

type DecodeData = (AddrMode, (AddrOffset, (Enabled ArgReg, (Bool, (Enabled ArgReg, (Bool, (OpClass, Bool)))))))

decode' :: Byte -> Maybe DecodeData
decode' 0x00 = special OpBRK -- BRK
decode' 0x01 = bin AddrIndirect OffsetPreAddX ORA -- ORA
decode' 0x02 = kill
decode' 0x03 = unsupported -- SLO
decode' 0x04 = unsupported -- NOP
decode' 0x05 = bin AddrZP OffsetNone ORA -- ORA
decode' 0x06 = un AddrZP OffsetNone ASL -- ASL
decode' 0x07 = unsupported -- RLA
decode' 0x08 = push StackArgP -- PHP
decode' 0x09 = bin AddrImm OffsetNone ORA -- ORA
decode' 0x0A = un AddrNone OffsetNone ASL -- ASL
decode' 0x0B = unsupported -- ANC
decode' 0x0C = unsupported -- NOP
decode' 0x0D = bin AddrDirect OffsetNone ORA -- ORA
decode' 0x0E = un AddrDirect OffsetNone ASL -- ASL
decode' 0x0F = unsupported -- SLO
decode' 0x10 = branch BranchN False -- BPL
decode' 0x11 = bin AddrIndirect OffsetPostAddY ORA -- ORA
decode' 0x12 = kill
decode' 0x13 = unsupported -- SLO
decode' 0x14 = unsupported -- NOP
decode' 0x15 = bin AddrZP OffsetPreAddX ORA -- ORA
decode' 0x16 = un AddrZP OffsetPreAddX ASL -- ASL
decode' 0x17 = unsupported -- SLO
decode' 0x18 = setFlag 0 False -- CLC
decode' 0x19 = bin AddrDirect OffsetPreAddY ORA -- ORA
decode' 0x1A = unsupported -- NOP
decode' 0x1B = unsupported -- SLO
decode' 0x1C = unsupported -- NOP
decode' 0x1D = bin AddrDirect OffsetPreAddX ORA -- ORA
decode' 0x1E = un AddrDirect OffsetPreAddX ASL -- ASL
decode' 0x1F = unsupported -- SLO
decode' 0x20 = jumpCall Call False -- JSR
decode' 0x21 = bin AddrIndirect OffsetPreAddX AND -- AND
decode' 0x22 = kill
decode' 0x23 = unsupported -- RLA
decode' 0x24 = bit AddrZP -- BIT
decode' 0x25 = bin AddrZP OffsetNone AND -- AND
decode' 0x26 = un AddrZP OffsetNone ROL -- ROL
decode' 0x27 = unsupported -- RLA
decode' 0x28 = pop StackArgP -- PLP
decode' 0x29 = bin AddrImm OffsetNone AND -- AND
decode' 0x2A = un AddrNone OffsetNone ROL -- ROL
decode' 0x2B = unsupported -- ANC
decode' 0x2C = bit AddrDirect -- BIT
decode' 0x2D = bin AddrDirect OffsetNone AND -- AND
decode' 0x2E = un AddrDirect OffsetNone ROL -- ROL
decode' 0x2F = unsupported -- RLA
decode' 0x30 = branch BranchN True -- BMI
decode' 0x31 = bin AddrIndirect OffsetPostAddY AND -- AND
decode' 0x32 = kill
decode' 0x33 = unsupported -- RLA
decode' 0x34 = unsupported -- NOP
decode' 0x35 = bin AddrZP OffsetPreAddX AND -- AND
decode' 0x36 = un AddrZP OffsetPreAddX ROL -- ROL
decode' 0x37 = unsupported -- RLA
decode' 0x38 = setFlag 0 True -- SEC
decode' 0x39 = bin AddrDirect OffsetPreAddY AND -- AND
decode' 0x3A = unsupported -- NOP
decode' 0x3B = unsupported -- RLA
decode' 0x3C = unsupported -- NOP
decode' 0x3D = bin AddrDirect OffsetPreAddX AND -- AND
decode' 0x3E = un AddrDirect OffsetPreAddX ROL -- ROL
decode' 0x3F = unsupported -- RLA
decode' 0x40 = special OpRTI
decode' 0x41 = bin AddrIndirect OffsetPreAddX EOR -- EOR
decode' 0x42 = kill
decode' 0x43 = unsupported -- SRE
decode' 0x44 = unsupported -- NOP
decode' 0x45 = bin AddrZP OffsetNone EOR -- EOR
decode' 0x46 = un AddrZP OffsetNone LSR -- LSR
decode' 0x47 = unsupported -- SRE
decode' 0x48 = push StackArgA -- PHA
decode' 0x49 = bin AddrImm OffsetNone EOR -- EOR
decode' 0x4A = un AddrNone OffsetNone LSR -- LSR
decode' 0x4B = unsupported -- ALR
decode' 0x4C = jumpCall Jump False -- JMP
decode' 0x4D = bin AddrDirect OffsetNone EOR -- EOR
decode' 0x4E = un AddrDirect OffsetNone LSR -- LSR
decode' 0x4F = unsupported -- SRE
decode' 0x50 = branch BranchV False -- BVC
decode' 0x51 = bin AddrIndirect OffsetPostAddY EOR -- EOR
decode' 0x52 = kill
decode' 0x53 = unsupported -- SRE
decode' 0x54 = unsupported -- NOP
decode' 0x55 = bin AddrZP OffsetPreAddX EOR -- EOR
decode' 0x56 = un AddrZP OffsetPreAddX LSR -- LSR
decode' 0x57 = unsupported -- SRE
decode' 0x58 = setFlag 2 False -- CLI
decode' 0x59 = bin AddrDirect OffsetPreAddY EOR -- EOR
decode' 0x5A = unsupported -- NOP
decode' 0x5B = unsupported -- SRE
decode' 0x5C = unsupported -- NOP
decode' 0x5D = bin AddrDirect OffsetPreAddX EOR -- EOR
decode' 0x5E = un AddrDirect OffsetPreAddX LSR -- LSR
decode' 0x5F = unsupported -- SRE
decode' 0x60 = special OpRTS
decode' 0x61 = bin AddrIndirect OffsetPreAddX ADC -- ADC
decode' 0x62 = kill
decode' 0x63 = unsupported -- RRA
decode' 0x64 = unsupported -- NOP
decode' 0x65 = bin AddrZP OffsetNone ADC -- ADC
decode' 0x66 = un AddrZP OffsetNone ROR -- ROR
decode' 0x67 = unsupported -- RRA
decode' 0x68 = pop StackArgA -- PLA
decode' 0x69 = bin AddrImm OffsetNone ADC -- ADC
decode' 0x6A = un AddrNone OffsetNone ROR -- ROR
decode' 0x6B = unsupported -- ALR
decode' 0x6C = jumpCall Jump True -- JMP
decode' 0x6D = bin AddrDirect OffsetNone ADC -- ADC
decode' 0x6E = un AddrDirect OffsetNone ROR -- ROR
decode' 0x6F = unsupported -- RRA
decode' 0x70 = branch BranchV True -- BVS
decode' 0x71 = bin AddrIndirect OffsetPostAddY ADC -- ADC
decode' 0x72 = kill
decode' 0x73 = unsupported -- RRA
decode' 0x74 = unsupported -- NOP
decode' 0x75 = bin AddrZP OffsetPreAddX ADC -- ADC
decode' 0x76 = un AddrZP OffsetPreAddX ROR -- ROR
decode' 0x77 = unsupported -- RRA
decode' 0x78 = setFlag 2 True -- SEI
decode' 0x79 = bin AddrDirect OffsetPreAddY ADC -- ADC
decode' 0x7A = unsupported -- NOP
decode' 0x7B = unsupported -- RRA
decode' 0x7C = unsupported -- NOP
decode' 0x7D = bin AddrDirect OffsetPreAddX ADC -- ADC
decode' 0x7E = un AddrDirect OffsetPreAddX ROR -- ROR
decode' 0x7F = unsupported -- RRA
decode' 0x80 = unsupported -- NOP
decode' 0x81 = sta AddrIndirect OffsetPreAddX -- STA
decode' 0x82 = unsupported -- NOP
decode' 0x83 = unsupported -- SAX
decode' 0x84 = sty AddrZP OffsetNone -- STY
decode' 0x85 = sta AddrZP OffsetNone -- STA
decode' 0x86 = stx AddrZP OffsetNone -- STX
decode' 0x87 = unsupported -- SAX
decode' 0x88 = unRR RegY RegY DEC -- DEY
decode' 0x89 = unsupported -- NOP
decode' 0x8A = unRR RegX RegA STX -- TXA
decode' 0x8B = unsupported -- XAA
decode' 0x8C = sty AddrDirect OffsetNone -- STY
decode' 0x8D = sta AddrDirect OffsetNone -- STA
decode' 0x8E = stx AddrDirect OffsetNone -- STX
decode' 0x8F = unsupported -- SAX
decode' 0x90 = branch BranchC False -- BCC
decode' 0x91 = sta AddrIndirect OffsetPostAddY -- STA
decode' 0x92 = kill
decode' 0x93 = unsupported -- AHX
decode' 0x94 = sty AddrZP OffsetPreAddX -- STY
decode' 0x95 = sta AddrZP OffsetPreAddX -- STA
decode' 0x96 = stx AddrZP OffsetPreAddY -- STX
decode' 0x97 = unsupported -- SAX
decode' 0x98 = unRR RegY RegA STX -- TYA
decode' 0x99 = sta AddrDirect OffsetPreAddY -- STA
decode' 0x9A = unRR RegX RegSP STX -- TXS
decode' 0x9B = unsupported -- TAS
decode' 0x9C = unsupported -- SHY
decode' 0x9D = sta AddrDirect OffsetPreAddX -- STA
decode' 0x9E = unsupported -- SHX
decode' 0x9F = unsupported -- AHX
decode' 0xA0 = ldy AddrImm OffsetNone -- LDY
decode' 0xA1 = lda AddrIndirect OffsetPreAddX -- LDA
decode' 0xA2 = ldx AddrImm OffsetNone -- LDX
decode' 0xA3 = unsupported -- LAX
decode' 0xA4 = ldy AddrZP OffsetNone -- LDY
decode' 0xA5 = lda AddrZP OffsetNone -- LDA
decode' 0xA6 = ldx AddrZP OffsetNone -- LDX
decode' 0xA7 = unsupported -- LAX
decode' 0xA8 = unRR RegA RegY STX -- TAY
decode' 0xA9 = lda AddrImm OffsetNone -- LDA
decode' 0xAA = unRR RegA RegX STX -- TAX
decode' 0xAB = unsupported -- LAX
decode' 0xAC = ldy AddrDirect OffsetNone -- LDY
decode' 0xAD = lda AddrDirect OffsetNone -- LDA
decode' 0xAE = ldx AddrDirect OffsetNone -- LDX
decode' 0xAF = unsupported -- LAX
decode' 0xB0 = branch BranchC True -- BCS
decode' 0xB1 = lda AddrIndirect OffsetPostAddY -- LDA
decode' 0xB2 = kill
decode' 0xB3 = unsupported -- LAX
decode' 0xB4 = ldy AddrZP OffsetPreAddX -- LDY
decode' 0xB5 = lda AddrZP OffsetPreAddX -- LDA
decode' 0xB6 = ldx AddrZP OffsetPreAddY -- LDX
decode' 0xB7 = unsupported -- LAX
decode' 0xB8 = setFlag 6 False -- CLV
decode' 0xB9 = lda AddrDirect OffsetPreAddY -- LDA
decode' 0xBA = unRR RegSP RegX STX -- TSX
decode' 0xBB = unsupported -- LAS
decode' 0xBC = ldy AddrDirect OffsetPreAddX -- LDY
decode' 0xBD = lda AddrDirect OffsetPreAddX -- LDA
decode' 0xBE = ldx AddrDirect OffsetPreAddY -- LDX
decode' 0xBF = unsupported -- LAX
decode' 0xC0 = cpy AddrImm OffsetNone -- CPY
decode' 0xC1 = cmp AddrIndirect OffsetPreAddX -- CMP
decode' 0xC2 = unsupported -- NOP
decode' 0xC3 = unsupported -- DCP
decode' 0xC4 = cpy AddrZP OffsetNone -- CPY
decode' 0xC5 = cmp AddrZP OffsetNone -- CMP
decode' 0xC6 = un AddrZP OffsetNone DEC -- DEC
decode' 0xC7 = unsupported -- DCP
decode' 0xC8 = unRR RegY RegY INC -- INY
decode' 0xC9 = cmp AddrImm OffsetNone -- CMP
decode' 0xCA = unRR RegX RegX DEC -- DEX
decode' 0xCB = unsupported -- AXS
decode' 0xCC = cpy AddrDirect OffsetNone -- CPY
decode' 0xCD = cmp AddrDirect OffsetNone -- CMP
decode' 0xCE = un AddrDirect OffsetNone DEC -- DEC
decode' 0xCF = unsupported -- DCP
decode' 0xD0 = branch BranchZ False -- BNE
decode' 0xD1 = cmp AddrIndirect OffsetPostAddY -- CMP
decode' 0xD2 = kill
decode' 0xD3 = unsupported -- DCP
decode' 0xD4 = unsupported -- NOP
decode' 0xD5 = cmp AddrZP OffsetPreAddX -- CMP
decode' 0xD6 = un AddrZP OffsetPreAddX DEC -- DEC
decode' 0xD7 = unsupported -- DCP
decode' 0xD8 = setFlag 3 False -- CLD
decode' 0xD9 = cmp AddrDirect OffsetPreAddY -- CMP
decode' 0xDA = unsupported -- NOP
decode' 0xDB = unsupported -- DCP
decode' 0xDC = unsupported -- NOP
decode' 0xDD = cmp AddrDirect OffsetPreAddX -- CMP
decode' 0xDE = un AddrDirect OffsetPreAddX DEC -- DEC
decode' 0xDF = unsupported -- DCP
decode' 0xE0 = cpx AddrImm OffsetNone -- CPX
decode' 0xE1 = bin AddrIndirect OffsetPreAddX SBC -- SBC
decode' 0xE2 = unsupported -- NOP
decode' 0xE3 = unsupported -- ISC
decode' 0xE4 = cpx AddrZP OffsetNone -- CPX
decode' 0xE5 = bin AddrZP OffsetNone SBC -- SBC
decode' 0xE6 = un AddrZP OffsetNone INC -- INC
decode' 0xE7 = unsupported -- ISC
decode' 0xE8 = unRR RegX RegX INC -- INX
decode' 0xE9 = bin AddrImm OffsetNone SBC -- SBC
decode' 0xEA = nop AddrNone -- NOP
decode' 0xEB = unsupported -- SBC
decode' 0xEC = cpx AddrDirect OffsetNone -- CPX
decode' 0xED = bin AddrDirect OffsetNone SBC -- SBC
decode' 0xEE = un AddrDirect OffsetNone INC -- INC
decode' 0xEF = unsupported -- ISC
decode' 0xF0 = branch BranchZ True -- BEQ
decode' 0xF1 = bin AddrIndirect OffsetPostAddY SBC -- SBC
decode' 0xF2 = kill
decode' 0xF3 = unsupported -- ISC
decode' 0xF4 = unsupported -- NOP
decode' 0xF5 = bin AddrZP OffsetPreAddX SBC -- SBC
decode' 0xF6 = un AddrZP OffsetPreAddX INC -- INC
decode' 0xF7 = unsupported -- ISC
decode' 0xF8 = setFlag 3 True -- SED
decode' 0xF9 = bin AddrDirect OffsetPreAddY SBC -- SBC
decode' 0xFA = unsupported -- NOP
decode' 0xFB = unsupported -- ISC
decode' 0xFC = unsupported -- NOP
decode' 0xFD = bin AddrDirect OffsetPreAddX SBC -- SBC
decode' 0xFE = un AddrDirect OffsetPreAddX INC -- INC
decode' 0xFF = unsupported -- ISC


special :: OpClass -> Maybe DecodeData
special op = return (AddrNone, (OffsetNone, (Nothing, (False, (Nothing, (False, (op, False)))))))

nop :: AddrMode -> Maybe DecodeData
nop addr = return (addr, (OffsetNone, (Nothing, (False, (Nothing, (False, (OpALU ALUCmp, False)))))))

bin :: AddrMode -> AddrOffset -> BinOp -> Maybe DecodeData
bin addr offset op = return (addr, (offset, (Just RegA, (addr /= AddrImm, (Just RegA, (False, (OpALU (ALUBin op), True)))))))
sta addr offset = return (addr, (offset, (Just RegA, (False, (Nothing, (True, (OpALU (ALUBin STA), False)))))))
lda addr offset = return (addr, (offset, (Nothing, (True, (Just RegA, (False, (OpALU (ALUBin LDA), True)))))))
cmp addr offset = return (addr, (offset, (Just RegA, (useMem, (Nothing, (False, (OpALU (ALUBin CMP), True)))))))
  where
    useMem = addr /= AddrImm

un addr offset op = return (addr, (offset, (reg, (not useA, (reg, (not useA, (OpALU (ALUUn op), True)))))))
  where
    useA = addr == AddrNone
    reg = if useA then Just RegA else Nothing
unRR r1 r2 op = return (AddrNone, (OffsetNone, (Just r1, (False, (Just r2, (False, (OpALU (ALUUn op), True)))))))
cpy = cmpR RegY
cpx = cmpR RegX
cmpR reg addr offset = return (addr, (offset, (Just reg, (useMem, (Nothing, (False, (OpALU ALUCmp, True)))))))
  where
    useMem = addr /= AddrImm
stx addr offset = return (addr, (offset, (Just RegX, (False, (Nothing, (True, (OpALU (ALUUn STX), False)))))))
ldx addr offset = return (addr, (offset, (Nothing, (not useImm, (Just RegX, (False, (OpALU (ALUUn LDX), True)))))))
  where
    useImm = addr == AddrImm
sty addr offset = return (addr, (offset, (Just RegY, (False, (Nothing, (True, (OpALU (ALUUn STX), False)))))))
ldy addr offset = return (addr, (offset, (Nothing, (not useImm, (Just RegY, (False, (OpALU (ALUUn LDX), True)))))))
  where
    useImm = addr == AddrImm

push arg = return (AddrNone, (OffsetNone, (Nothing, (False, (Nothing, (False, (OpPushPop Push arg, False)))))))
pop arg = return (AddrNone, (OffsetNone, (Nothing, (False, (targetReg, (False, (OpPushPop Pop arg, arg /= StackArgP)))))))
  where
    targetReg = case arg of
        StackArgA -> Just RegA
        StackArgP -> Nothing

branch flag when = return (AddrImm, (OffsetNone, (Nothing, (False, (Nothing, (False, (OpBranch flag when, False)))))))
setFlag flag val = return (AddrNone, (OffsetNone, (Nothing, (False, (Nothing, (False, (OpFlag flag val, False)))))))
jumpCall jc indirect = return (AddrDirect, (OffsetNone, (Nothing, (indirect, (Nothing, (False, (OpJumpCall jc, False)))))))
bit addr = return (addr, (OffsetNone, (Nothing, (True, (Nothing, (False, (OpALU ALUBIT, True)))))))

kill = Nothing
unsupported = Nothing

unpackDecoded :: Signal clk DecodeData -> Decoded clk
unpackDecoded (unpack -> (dAddrMode, unpack -> (dAddrOffset, unpack -> (dSourceReg, unpack -> (dReadMem, unpack -> (dTargetReg, unpack -> (dWriteMem, unpack -> (dOp, dUpdateFlags)))))))) = Decoded{..}

decode :: forall clk. (Clock clk) => Signal clk Byte -> Decoded clk
decode op = Decoded{..}
  where
    -- (opAAA, opBBBCC) = swap . unappendS $ op :: (Signal clk U3, Signal clk U5)
    -- (opBBB, opCC) = swap . unappendS $ opBBBCC :: (Signal clk U3, Signal clk U2)
    (opAAABBB, opCC) = swap . unappendS $ op :: (Signal clk U6, Signal clk U2)
    (opAAA, opBBB) = swap . unappendS $ opAAABBB :: (Signal clk U3, Signal clk U3)
    isBinOp = opCC .==. [b|01|]
    binOp = bitwise opAAA
    isUnOp = opCC .==. [b|10|] .&&.
             -- bitNot (unOp `elemS` [Un_Special_1, Un_Special_2]) .&&.
             bitNot isNOP
    unOp = bitwise opAAA
    isUnA = unOp `elemS` [ASL, ROL, LSR, ROR, LDX] .&&. opBBB .==. [b|010|]
    isUnX = unOp `elemS` [DEC] .&&. opBBB .==. [b|010|]

    isSTY = opCC .==. [b|00|] .&&. opAAA .==. [b|100|]
    isTXA = op .==. 0x8A
    isTXS = op .==. 0x9a
    isTSX = op .==. 0xba
    isLDY = opCC .==. [b|00|] .&&. opAAA .==. [b|101|]
    isNOP = op .==. 0xEA
    dJSR = op .==. 0x20
    dJump = op `elemS` [0x4C, 0x6C]
    dRTS = op .==. 0x60
    dBRK = op .==. 0x00
    dRTI = op .==. 0x40
    dBIT = op `elemS` [0x24, 0x2C]

    dPush = op `elemS` [0x48, 0x08]
    dPop = op `elemS` [0x68, 0x28]

    dAddrMode = muxN [ (addrNone, pureS AddrNone)
                     , (addrImm, pureS AddrImm)
                     , (addrZP, pureS AddrZP)
                     , (addrIndirect, pureS AddrIndirect)
                     , (addrDirect, pureS AddrDirect)
                     ]

    dAddrOffset = muxN [ (addrPreAddX, pureS OffsetPreAddX)
                       , (addrPreAddY, pureS OffsetPreAddY)
                       , (addrPostAddY, pureS OffsetPostAddY)
                       , (high, pureS OffsetNone)
                       ]

    Addressing{..} = Addressing{..}
      where
        addrNone = muxN [ (isBinOp, low)
                        , (isUnOp, isUnA .||. isUnX .||. isTXA .||. isTXS .||. isTSX)
                        , (isBranch .||. dJump, low)
                        , (opCC .==. [b|00|] .&&. opAAA ./=. [b|000|],
                             bitNot $ opBBB `elemS` [[b|000|], [b|001|], [b|011|], [b|101|], [b|111|]])
                        , (high, high)
                        ]
        addrImm = muxN [ (isBinOp, opBBB .==. [b|010|])
                       , (isUnOp, opBBB .==. [b|000|])
                       , (isBranch, high)
                       , (opCC .==. [b|00|], (bitNot (dJSR .||. dRTS) .&&. opBBB .==. [b|000|]))
                       , (high, low)
                       ]
        addrZP = opBBB `elemS` [[b|001|], [b|101|]]

        useY = isUnOp .&&. opAAA `elemS` [[b|100|], [b|101|]] -- STX/LDX
        preAddX = muxN [ (isBinOp .||. isUnOp, opBBB `elemS` [[b|000|], [b|101|], [b|111|]])
                       , (opCC .==. [b|00|], opBBB `elemS` [[b|101|], [b|111|]])
                       , (high, low)
                       ]
        addrPreAddX = mux useY (preAddX, low)
        preAddY = isBinOp .&&. opBBB .==. [b|110|]
        addrPreAddY = mux useY (preAddY, preAddX)
        addrPostAddY = isBinOp .&&. opBBB .==. [b|100|]
        addrIndirect = muxN [ (isBinOp, opBBB `elemS` [[b|000|], [b|100|]])
                            , (high, low)
                            ]
        addrDirect = muxN [ (isBinOp, opBBB `elemS` [[b|011|], [b|110|], [b|111|]])
                          , (high,  dJSR .||. opBBB `elemS` [[b|011|], [b|111|]])
                          ]

    dALU = muxN [ (isEnabled dUseBinALU, enabledS . funMap (return . ALUBin) . enabledVal $ dUseBinALU)
                , (isEnabled dUseUnALU, enabledS . funMap (return . ALUUn) . enabledVal $ dUseUnALU)
                , (dUseCmpALU, enabledS . pureS $ ALUCmp)
                , (dBIT, enabledS . pureS $ ALUBIT)
                , (isNOP, enabledS $ pureS $ ALUUn LDX)
                , (high, disabledS)
                ]

    dUseBinALU = packEnabled isBinOp binOp
    dUseUnALU = muxN [ (isUnOp, enabledS unOp)
                     , (op `elemS` [0xE8, 0xC8], enabledS $ pureS INC) -- INX, INY
                     , (op `elemS` [0xCA, 0x88], enabledS $ pureS DEC) -- DEX, DEY
                     , (op .==. pureS 0xA8, enabledS $ pureS LDX) -- TAY
                     , (op .==. pureS 0x98, enabledS $ pureS STX) -- TYA
                     , (isSTY, enabledS $ pureS STX)
                     , (isLDY, enabledS $ pureS LDX)
                     , (high, disabledS)
                     ]
    dUseCmpALU = opCC .==. [b|00|] .&&.
                 opAAA `elemS` [[b|110|], [b|111|]] .&&.
                 opBBB `elemS` [[b|000|], [b|001|], [b|011|]]

    dSourceReg = muxN [ (dReadX, enabledS . pureS $ RegX)
                      , (dReadY, enabledS . pureS $ RegY)
                      , (dReadA, enabledS . pureS $ RegA)
                      , (dReadSP, enabledS . pureS $ RegSP)
                      , (high, disabledS)
                      ]
    dReadA = muxN [ (isBinOp, binOp ./=. pureS LDA)
                  , (isUnOp, isUnA)
                  , (op .==. pureS 0xA8, high) -- TAY
                  , (high, low)
                  ]
    dReadX = muxN [ (isBinOp, low)
                  , (isUnOp, unOp .==. pureS STX .||. isUnX)
                  , (op `elemS` [0xE8, 0xCA], high) -- INX
                  , (dUseCmpALU, opAAA .==. [b|111|])
                  , (high, low)
                  ]
    dReadY = muxN [ (isBinOp, low)
                  , (isUnOp, low)
                  , (op `elemS` [0xC8, 0x88], high) -- INY, DEY
                  , (op .==. pureS 0x98, high) -- TYA
                  , (dUseCmpALU, opAAA .==. [b|110|])
                  , (isSTY, high)
                  , (high, low)
                  ]
    dReadSP = isTSX
    dReadMem = muxN [ (isBinOp, bitNot $ binOp .==. pureS STA .||. addrImm)
                    , (isUnOp, bitNot $ dReadA .||. dReadX .||. dReadY .||. dReadSP)
                    , (dUseCmpALU, opAAA `elemS` [[b|110|], [b|111|]] .&&.
                                   bitNot addrImm)
                    , (isLDY, bitNot dReadA)
                    , (dBIT, high)
                    , (high, op .==. 0x6C) -- indirect JMP
                    ]

    dTargetReg = muxN [ (dWriteX, enabledS . pureS $ RegX)
                      , (dWriteY, enabledS . pureS $ RegY)
                      , (dWriteA, enabledS . pureS $ RegA)
                      , (dWriteSP, enabledS . pureS $ RegSP)
                      , (high, disabledS)
                      ]

    dWriteA = muxN [ (isBinOp, bitNot $ binOp `elemS` [STA, CMP])
                   , (isUnOp, isUnA .||. isTXA)
                   , (op .==. pureS 0x98, high) -- TYA
                   , (op .==. pureS 0x68, high) -- PLA
                   , (high, low)
                   ]
    dWriteX = muxN [ (isBinOp, low)
                   , (isUnOp, unOp .==. pureS LDX .||. isUnX)
                   , (op `elemS` [0xE8, 0xCA], high) -- INX
                   , (high, low)
                   ]
    dWriteY = muxN [ (isBinOp, low)
                   , (isUnOp, low)
                   , (op `elemS` [0xC8, 0x88, 0xA8], high) -- INY, DEY, TAY
                   , (isLDY, high)
                   , (high, low)
                   ]
    dWriteSP = isTXS
    dWriteMem = muxN [ (isBinOp, binOp .==. pureS STA)
                     , (isUnOp, unOp ./=. pureS LDX .&&. bitNot isUnA .&&. bitNot isUnX)
                     , (isSTY, bitNot dWriteA)
                     , (high, low)
                     ]

    dUpdateFlags = muxN [ (isBinOp, binOp ./=. pureS STA)
                        , (isUnOp, bitNot $ unOp .==. pureS STX .&&. bitNot (opBBB `elemS` [[b|010|], [b|110|]]))
                        , (dUseCmpALU, high)
                        , (isLDY, high)
                        , (op `elemS` [0xE8, 0xC8], high) -- INX, INY
                        , (op `elemS` [0xCA, 0x88], high) -- DEX, DEY
                        , (op `elemS` [0x98], high) -- TYA
                        , (dPop, op .==. 0x68)
                        , (dBIT, high)
                        , (high, low)
                        ]

    isBranch = opCC .==. [b|00|] .&&. opBBB .==. [b|100|]
    dBranch = packEnabled isBranch $ pack (selector, targetValue)
      where
        (selector, targetValue) = swap . unappendS $ opAAA

    isChangeFlag = opBBB .==. [b|110|] .&&. opCC .==. [b|00|] .&&. opAAA ./=. [b|100|]
    setFlag = opAAA `elemS` [[b|001|], [b|011|], [b|111|]]
    flag = switchS opAAA [ ([b|000|], 0) -- C
                         , ([b|001|], 0) -- C
                         , ([b|010|], 2) -- I
                         , ([b|011|], 2) -- I
                         , ([b|101|], 6) -- V
                         , ([b|110|], 3) -- D
                         , ([b|111|], 3) -- D
                         ]

    dWriteFlag = packEnabled isChangeFlag $ pack (flag, setFlag)

    dOp = muxN [ muxMatch dBranch $ funMap (return . uncurry OpBranch)
               , muxMatch dWriteFlag $ funMap (return . uncurry OpFlag)
               , muxMatch dALU $ funMap (return . OpALU)
               , (dJump, pureS $ OpJumpCall Jump)
               , (dPush, funMap (return . OpPushPop Push) $ mux (op .==. 0x48) (pureS StackArgP, pureS StackArgA))
               , (dPop, funMap (return . OpPushPop Pop) $ mux (op .==. 0x68) (pureS StackArgP, pureS StackArgA))
               , (dJSR, pureS $ OpJumpCall Call)
               , (dRTS, pureS OpRTS)
               , (dBRK, pureS OpBRK)
               , (dRTI, pureS OpRTI)
               ]
