{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module MOS6502.CPU where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU

import Language.Literals.Binary
import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Control.Monad ((<=<), void)
import Data.Tuple (swap)

data CPUIn clk = CPUIn
    { cpuMemR :: Signal clk Byte
    -- , cpuDBE :: Signal clk Bool
    -- , cpuRDY :: Signal clk Bool
    , cpuIRQ :: Signal clk ActiveLow
    , cpuNMI :: Signal clk ActiveLow
    -- , cpuSO :: Signal clk ActiveLow
    , cpuWait :: Signal clk Bool -- XXX KLUDGE
    }

data CPUOut clk = CPUOut
    { cpuMemA :: Signal clk Addr
    , cpuMemW :: Signal clk (Enabled Byte)
    -- , cpuSync :: Signal clk Bool
    }

data CPUDebug clk = CPUDebug
    { cpuState :: Signal clk State
    , cpuArgBuf :: Signal clk Byte
    , cpuA :: Signal clk Byte
    , cpuX :: Signal clk Byte
    , cpuY :: Signal clk Byte
    , cpuP :: Signal clk Byte
    , cpuSP :: Signal clk Byte
    , cpuPC :: Signal clk Addr
    , cpuOp :: Signal clk Byte
    , cpuDecoded :: Decoded clk
    }

data State = Halt
           | Init
           | FetchVector1
           | FetchVector2
           | Fetch1
           | Fetch2
           | Fetch3
           | Indirect1
           | Indirect2
           | WaitRead
           | WaitPushAddr
           | WaitWrite
           deriving (Show, Eq, Enum, Bounded)
type StateSize = X12

instance Rep State where
    type W State = X4 -- W StateSize
    newtype X State = XState{ unXState :: Maybe State }

    unX = unXState
    optX = XState
    toRep s = toRep . optX $ s'
      where
        s' :: Maybe StateSize
        s' = fmap (fromIntegral . fromEnum) $ unX s
    fromRep rep = optX $ fmap (toEnum . fromIntegral . toInteger) $ unX x
      where
        x :: X StateSize
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness StateSize)

bitsToByte :: (Clock clk)
           => Matrix X8 (Signal clk Bool)
           -> Signal clk Byte
bitsToByte = bitwise . packMatrix

byteToBits :: (Clock clk)
           => Signal clk Byte
           -> Matrix X8 (Signal clk Bool)
byteToBits = unpackMatrix . bitwise

data Addressing clk = Addressing{ addrNone :: Signal clk Bool
                                , addrImm :: Signal clk Bool
                                , addrZP :: Signal clk Bool
                                , addrPreAddX :: Signal clk Bool
                                , addrPreAddY :: Signal clk Bool
                                , addrPostAddY :: Signal clk Bool
                                , addrIndirect :: Signal clk Bool
                                , addrDirect :: Signal clk Bool
                                , addrPop :: Signal clk Bool
                                }

data Decoded clk = Decoded{ dAddr :: Addressing clk
                          , dReadMem :: Signal clk Bool
                          , dReadA :: Signal clk Bool
                          , dReadX :: Signal clk Bool
                          , dReadY :: Signal clk Bool
                          , dUseBinALU :: Signal clk (Enabled BinOp)
                          , dUseUnALU :: Signal clk (Enabled UnOp)
                          , dUseCmpALU :: Signal clk Bool
                          , dUpdateFlags :: Signal clk Bool
                          , dWriteA :: Signal clk Bool
                          , dWriteX :: Signal clk Bool
                          , dWriteY :: Signal clk Bool
                          , dWriteMem :: Signal clk Bool
                          , dWriteFlags :: Signal clk Bool
                          , dBranch :: Signal clk (Enabled (U2, Bool))
                          , dSetFlag :: Signal clk (Enabled X8)
                          , dClearFlag :: Signal clk (Enabled X8)
                          , dJump :: Signal clk Bool
                          , dJSR :: Signal clk Bool
                          , dRTS :: Signal clk Bool
                          }

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
    isShift = isUnOp .&&. unOp `elemS` [ASL, ROL, LSR, ROR, LDX]
    isUnAcc = isShift .&&. opBBB .==. [b|010|]

    isSTY = opCC .==. [b|00|] .&&. opAAA .==. [b|100|]
    isLDY = opCC .==. [b|00|] .&&. opAAA .==. [b|101|]
    dJSR = op .==. 0x20
    dJump = op `elemS` [0x4C, 0x6C]
    dRTS = op .==. 0x60

    dAddr = Addressing{..}
      where
        addrNone = muxN [ (isBinOp, low)
                        , (isUnOp, isUnAcc)
                        , (isBranch .||. dJump, low)
                        , (opCC .==. [b|00|] .&&. opAAA ./=. [b|000|],
                             bitNot $ opBBB `elemS` [[b|000|], [b|001|], [b|011|], [b|101|], [b|111|]])
                        , (high, high)
                        ]
        addrImm = muxN [ (isBinOp, opBBB .==. [b|010|])
                       , (isUnOp, opBBB .==. [b|000|])
                       , (opCC .==. [b|00|], bitNot dJSR .&&. opBBB .==. [b|000|])
                       , (isBranch .||. dJump, high)
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
                            , (high, op `elemS` [0x6C]) -- JMP
                            ]
        addrDirect = muxN [ (isBinOp, opBBB `elemS` [[b|011|], [b|110|], [b|111|]])
                          , (high,  dJSR .||. opBBB `elemS` [[b|011|], [b|111|]])
                          ]
        addrPop = op `elemS` [ 0x68, 0x28 ] -- PLA, PLP

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

    dReadA = muxN [ (isBinOp, binOp ./=. pureS LDA)
                  , (isUnOp, isUnAcc)
                  , (op .==. pureS 0xA8, high) -- TAY
                  , (high, low)
                  ]
    dReadX = muxN [ (isBinOp, low)
                  , (isUnOp, unOp .==. pureS STX)
                  , (op `elemS` [0xE8, 0xCA], high) -- INX, DEX
                  , (dUseCmpALU, opAAA .==. [b|111|])
                  , (high, low)
                  ]
    dReadY = muxN [ (isBinOp, low)
                  , (isUnOp, low)
                  , (op `elemS` [0xC8, 0x88], high) -- INY, DEY
                  , (op .==. pureS 0x98, high) -- TYA
                  , (dUseCmpALU, opAAA .==. [b|110|])
                  , (high, low)
                  ]
    dReadMem = muxN [ (isBinOp, bitNot dReadA)
                    , (isUnOp, bitNot $ dReadA .||. dReadX)
                    , (high, low)
                    ]

    dWriteA = muxN [ (isBinOp, binOp ./=. pureS STA)
                   , (isUnOp, isUnAcc)
                   , (op .==. pureS 0x98, high) -- TYA
                   , (op .==. pureS 0x68, high) -- PLA
                   , (high, low)
                   ]
    dWriteX = muxN [ (isBinOp, low)
                   , (isUnOp, unOp .==. pureS LDX)
                   , (op `elemS` [0xE8, 0xCA], high) -- INX, DEX
                   , (high, low)
                   ]
    dWriteY = muxN [ (isBinOp, low)
                   , (isUnOp, low)
                   , (op `elemS` [0xC8, 0x88], high) -- INY, DEY
                   , (op .==. pureS 0xA8, high) -- TAY
                   , (high, low)
                   ]
    dWriteMem = muxN [ (isBinOp, binOp .==. pureS STA)
                     , (isUnOp, unOp ./=. pureS LDX .&&. bitNot isUnAcc)
                     , (high, low)
                     ]
    dWriteFlags = op .==. 0x28 -- PLP

    dUpdateFlags = muxN [ (isBinOp, binOp ./=. pureS STA)
                        , (isUnOp, unOp ./=. pureS STX)
                        , (op `elemS` [0xE8, 0xC8], high) -- INX, INY
                        , (op `elemS` [0xCA, 0x88], high) -- DEX, DEY
                        , (high, low)
                        ]

    isBranch = opCC .==. [b|00|] .&&. opBBB .==. [b|100|]
    dBranch = packEnabled isBranch $ pack (selector, targetValue)
      where
        selector = unsigned $ opAAA `shiftR` 1
        targetValue = opAAA `testABit` 0

    isChangeFlag = opBBB .==. [b|110|] .&&. opCC .==. [b|00|] .&&. opAAA ./=. [b|100|]
    setFlag = opAAA `elemS` [[b|001|], [b|011|], [b|111|]]
    clearFlag = bitNot setFlag
    flag = switchS opAAA [ ([b|000|], 0) -- C
                         , ([b|001|], 0) -- C
                         , ([b|010|], 2) -- I
                         , ([b|011|], 2) -- I
                         , ([b|101|], 6) -- V
                         , ([b|110|], 3) -- D
                         , ([b|111|], 3) -- D
                         ]
    dSetFlag = packEnabled (isChangeFlag .&&. setFlag) flag
    dClearFlag = packEnabled (isChangeFlag .&&. clearFlag) flag

cpu :: forall clk. (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu CPUIn{..} = runRTL $ do
    -- State
    s <- newReg Init
    rOp <- newReg 0x00
    let op = var rOp
        decoded@Decoded{..} = decode op
        Addressing{..} = dAddr
        size2 = addrImm .||. addrZP
        size3 = addrDirect .||. addrIndirect
        size1 = addrNone -- bitNot $ size2 .||. size3

    rArgBuf <- newReg 0x00
    let argByte = cpuMemR
    let argWord = reg rArgBuf `appendS` argByte

    -- Registers
    rA <- newReg 0x00
    rX <- newReg 0x00
    rY <- newReg 0x00
    rSP <- newReg 0xFF
    rPC <- newReg 0x0000 -- To be filled in by Init
    let popTarget = 0x0100 .|. unsigned (reg rSP + 1)
        pushTarget = 0x0100 .|. unsigned (reg rSP)

    -- Flags
    fC <- newReg False
    fZ <- newReg False
    fI <- newReg False
    fD <- newReg False
    fB <- newReg False
    fV <- newReg False
    fN <- newReg False

    let flags = bitsToByte . Matrix.fromList $
                [ reg fC
                , reg fZ
                , reg fI
                , reg fD
                , reg fB
                , high
                , reg fV
                , reg fN
                ]
        setFlags mtx = do
            let [c, z, i, d, b, _, v, n] = Matrix.toList . byteToBits $ mtx
            fC := c
            fZ := z
            fI := i
            fD := d
            fB := b
            fV := v
            fN := n

        branchFlags :: Signal clk (Matrix U2 Bool)
        branchFlags = pack . Matrix.fromList . map reg $ [fN, fV, fC, fZ]

    rNextA <- newReg 0x0000
    rNextW <- newReg Nothing

    let aluIn = ALUIn{ aluInC = reg fC, aluInD = reg fD }

    let unArg = muxN [ (dReadMem, argByte)
                     , (dReadA, reg rA)
                     , (dReadX, reg rX)
                     , (dReadY, reg rY)
                     ]

    let cmpArg = muxN [ (dReadX, reg rX)
                      , (dReadY, reg rY)
                      ]
        (cmpOut, cmpRes) = cmpALU cmpArg argByte

    let (binOut, binRes) = binaryALU (enabledVal dUseBinALU) aluIn (reg rA) argByte
        (unOut, unRes) = unaryALU (enabledVal dUseUnALU) aluIn unArg
    let res = muxN [ (isEnabled dUseBinALU, binRes)
                   , (isEnabled dUseUnALU, unRes)
                   , (dUseCmpALU, cmpRes)
                   ]

    let commitALUFlags = do
            CASE [ IF (isEnabled dUseBinALU) $ commit binOut
                 , IF (isEnabled dUseUnALU) $ commit unOut
                 , IF dUseCmpALU $ commit cmpOut
                 ]
            fZ := res .==. 0
            fN := res `testABit` 7
          where
            commit ALUOut{..} = do
                CASE [ match aluOutC (fC :=) ]
                CASE [ match aluOutV (fV :=) ]

    let addrPreOffset = muxN [ (addrPreAddX, reg rX)
                             , (addrPreAddY, reg rY)
                             , (high, 0)
                             ]
    let addr1 = muxN [ (addrZP, unsigned $ argByte + addrPreOffset)
                     , (addrDirect, argWord + unsigned addrPreOffset)
                     ]
        run1 = do
            CASE [ match dBranch $ \branch -> do
                        let (selector, target) = unpack branch
                        let branchFlag = branchFlags .!. selector
                            branchCond = branchFlag .==. target
                        WHEN branchCond $ do
                            rPC := reg rPC + signed argByte
                        s := pureS Fetch1
                 , IF dJump $ do
                        CASE [ IF addrIndirect $ do
                                    rNextA := addr1
                                    s := pureS FetchVector1
                             , OTHERWISE $ do
                                    rPC := addr1
                                    s := pureS Fetch1
                             ]
                 , IF addrPop $ do
                        rSP := reg rSP + 1
                        rNextA := popTarget
                        s := pureS WaitRead
                 , IF (addrNone .||. addrImm) $ do
                        WHEN dUpdateFlags $ commitALUFlags
                        WHEN dWriteA $ rA := res
                        WHEN dWriteX $ rX := res
                        WHEN dWriteY $ rY := res
                        s := pureS Fetch1
                 , IF dWriteMem $ do
                        rNextA := addr1
                        rNextW := enabledS res
                        s := pureS WaitWrite
                 , IF dJSR $ do
                        rArgBuf := unsigned (reg rPC)
                        rSP := reg rSP - 2
                        rNextA := pushTarget
                        rNextW := enabledS $ unsigned (reg rPC `shiftR` 8)
                        rPC := addr1
                        s := pureS WaitPushAddr
                 , OTHERWISE $ do
                        rNextA := addr1
                        s := pureS WaitRead
                 ]

    let addrPostOffset = muxN [ (addrPostAddY, reg rY)
                              , (high, 0)
                              ]
    let addr2 = argWord + unsigned addrPostOffset
    let run2 = do
            CASE [ IF dWriteMem $ do
                        rNextA := addr2
                        rNextW := enabledS res
                        s := pureS WaitWrite
                 , IF (op `elemS` [0x24, 0x2C]) $ do -- BIT
                        fZ := (reg rA .&. argByte) .==. 0
                        fV := argByte `testABit` 6
                        fN := argByte `testABit` 7
                 , IF addrIndirect $ do
                        rNextA := addr2
                        s := pureS WaitRead
                 , OTHERWISE $ do
                        WHEN dUpdateFlags $ commitALUFlags
                        WHEN dWriteFlags $ setFlags argByte
                        WHEN dWriteA $ rA := res
                        WHEN dWriteX $ rX := res
                        WHEN dWriteY $ rY := res
                        rNextA := reg rPC
                        s := pureS Fetch1
                 ]

    WHEN (bitNot cpuWait) $
      switch (reg s) $ \state -> case state of
          Init -> do
              rNextA := pureS resetVector
              s := pureS FetchVector1
          FetchVector1 -> do
              rPC := unsigned cpuMemR
              rNextA := reg rNextA + 1
              s := pureS FetchVector2
          FetchVector2 -> do
              let pc' = (reg rPC .&. 0xFF) .|. (unsigned cpuMemR `shiftL` 8)
              rPC := mux dRTS (pc', pc' + 1) -- BWAAAAH!
              rNextA := var rPC
              s := pureS Fetch1
          Fetch1 -> do
              rOp := cpuMemR
              CASE [ IF (op .==. 0x00) $ do -- TODO: BRK
                          s := pureS Halt
                   , IF dRTS $ do
                          rSP := reg rSP + 2
                          rNextA := popTarget
                          s := pureS FetchVector1
                   , IF (op `elemS` [0x48, 0x08]) $ do -- PHA, PHP
                          rSP := reg rSP - 1
                          rNextA := pushTarget
                          rNextW := enabledS $ mux (op .==. 0x08) (reg rA, flags)
                          s := pureS WaitWrite
                   , IF (op `elemS` [0x68, 0x28]) $ do -- PLA, PLP
                          rSP := reg rSP + 1
                          rNextA := popTarget
                          s := pureS WaitRead
                   , OTHERWISE $ do
                          WHEN (op .==. 0x00) $ s := pureS Halt
                          WHEN size1 run1
                   ]
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch2
          Fetch2 -> do
              WHEN size2 run1
              rPC := reg rPC + 1
              rArgBuf := cpuMemR
              rNextA := var rPC
              s := pureS Fetch3
          Fetch3 -> do
              run1
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch1
          Indirect1 -> do
              rArgBuf := cpuMemR
              rNextA := reg rNextA + 1
              s := pureS Indirect2
          Indirect2 -> do
              run2
              s := pureS Halt
          WaitRead -> do
              run2
              rNextA := reg rPC
              s := pureS Fetch1
          WaitPushAddr -> do
              rNextA := reg rNextA - 1
              rNextW := enabledS (reg rArgBuf)
              s := pureS WaitWrite
          WaitWrite -> do
              rNextW := disabledS
              rNextA := reg rPC
              s := pureS Fetch1
          _ -> do
              s := pureS Halt

    let cpuMemA = var rNextA
        cpuMemW = var rNextW

    -- Debug view
    let cpuState = reg s
        cpuOp = op
        cpuArgBuf = reg rArgBuf
        cpuDecoded = decoded
    let cpuA = reg rA
        cpuX = reg rX
        cpuY = reg rY
        cpuSP = reg rSP
        cpuP = flags
        cpuPC = reg rPC

    return (CPUOut{..}, CPUDebug{..})

resetVector :: Addr
resetVector = 0xFFFC

nmiVector :: Addr
nmiVector = 0xFFFA

irqVector :: Addr
irqVector = 0xFFFE
