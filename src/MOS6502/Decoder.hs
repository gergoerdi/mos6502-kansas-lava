{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module MOS6502.Decoder
       ( Addressing(..), Decoded(..), decode
       , ArgReg(..), BranchFlag(..)
       , ALUOp(..), aluBinOp, aluUnOp
       ) where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU

import Language.Literals.Binary
import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Tuple (swap)

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
$(repBitRep ''BranchFlag 2)

instance BitRep BranchFlag where
    bitRep = bitRepEnum

data ArgReg = RegA
            | RegX
            | RegY
            | RegSP
            deriving (Eq, Ord, Show, Enum, Bounded)
$(repBitRep ''ArgReg 2)

instance BitRep ArgReg where
    bitRep = bitRepEnum

data ALUOp = ALUBin BinOp
           | ALUUn UnOp
           | ALUCmp
           | ALUBIT
           deriving (Eq, Ord, Show)
$(repBitRep ''ALUOp 5)

instance BitRep ALUOp where
    bitRep = concat [ [(ALUBin bin, bits "00" & rep) | (bin, rep) <- bitRep]
                    , [(ALUUn un, bits "01" & rep) | (un, rep) <- bitRep]
                    , [(ALUCmp, bits "10000")]
                    , [(ALUBIT, bits "11000")]
                    ]

aluBinOp :: forall clk. Signal clk ALUOp -> Signal clk (Enabled BinOp)
aluBinOp s = muxN [ (sel .==. [b|00|], enabledS bin)
                  , (high, disabledS)
                  ]
  where
    (sel :: Signal clk X4, bin) = swap . unappendS $ s

aluUnOp :: forall clk. Signal clk ALUOp -> Signal clk (Enabled UnOp)
aluUnOp s = muxN [ (sel .==. [b|01|], enabledS un)
                 , (high, disabledS)
                 ]
  where
    (sel :: Signal clk X4, un) = swap . unappendS $ s

data OpClass = OpALU ALUOp
             | OpBranch BranchFlag Bool
             | OpJump
             | OpPush
             | OpPop
             | OpJSR
             | OpRTS
             | OpBRK
             | OpRTI
             deriving (Eq, Ord, Show)
$(repBitRep ''OpClass 9)

instance BitRep OpClass where
    bitRep = concat [ [(OpALU aluOp, bits "0000" & rep) | (aluOp, rep) <- bitRep]
                    , withBool [(OpBranch flag, bits "0001" & repFlag) | (flag, repFlag) <- bitRep]
                    , [ (cls, bs & bits "00000")
                      | (i, cls) <- zip [2 :: U4 ..] atomicClasses
                      , let bs = BitPat . toRep . optX . Just $ i :: BitPat X4
                      ]
                    ]
      where
        atomicClasses = [OpJump, OpPush, OpPop, OpJSR, OpRTS, OpBRK, OpRTI]

        withBool = concatMap $ \(mkValue, rep) ->
          [(mkValue b, rep & b')
          | b <- [True, False]
          , let b' = bits (if b then "1" else "0") :: BitPat X1
          ]

data Decoded clk = Decoded{ dAddr :: Addressing clk
                          , dSourceReg :: Signal clk (Enabled ArgReg)
                          , dReadMem :: Signal clk Bool
                          , dALU :: Signal clk (Enabled ALUOp)
                          -- , dUseBinALU :: Signal clk (Enabled BinOp)
                          -- , dUseUnALU :: Signal clk (Enabled UnOp)
                          -- , dUseCmpALU :: Signal clk Bool
                          , dUpdateFlags :: Signal clk Bool
                          , dTargetReg :: Signal clk (Enabled ArgReg)
                          , dWriteMem :: Signal clk Bool
                          , dBranch :: Signal clk (Enabled (BranchFlag, Bool))
                          , dWriteFlag :: Signal clk (Enabled (X8, Bool))
                          , dWriteFlags :: Signal clk Bool
                          , dJump :: Signal clk Bool
                          , dPush :: Signal clk Bool
                          , dPop :: Signal clk Bool
                          , dJSR :: Signal clk Bool
                          , dRTS :: Signal clk Bool
                          , dBRK :: Signal clk Bool
                          , dRTI :: Signal clk Bool
                          -- , dBIT :: Signal clk Bool
                          }
                 deriving Show

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
             bitNot (op .==. 0xEA) -- NOP
    unOp = bitwise opAAA
    isUnA = unOp `elemS` [ASL, ROL, LSR, ROR, LDX] .&&. opBBB .==. [b|010|]
    isUnX = unOp `elemS` [DEC] .&&. opBBB .==. [b|010|]

    isSTY = opCC .==. [b|00|] .&&. opAAA .==. [b|100|]
    isTXA = op .==. 0x8A
    isTXS = op .==. 0x9a
    isTSX = op .==. 0xba
    isLDY = opCC .==. [b|00|] .&&. opAAA .==. [b|101|]
    dJSR = op .==. 0x20
    dJump = op `elemS` [0x4C, 0x6C]
    dRTS = op .==. 0x60
    dBRK = op .==. 0x00
    dRTI = op .==. 0x40
    dBIT = op `elemS` [0x24, 0x2C]

    dPush = op `elemS` [0x48, 0x08]
    dPop = op `elemS` [0x68, 0x28]

    dAddr@Addressing{..} = Addressing{..}
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
    dWriteFlags = op .==. 0x28 -- PLP

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
