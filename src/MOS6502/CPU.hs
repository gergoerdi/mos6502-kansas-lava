{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MOS6502.CPU where

import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU
import MOS6502.Decoder

import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Data.Default

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
           | InitTest
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
type StateSize = X13

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

data CPUInit = CPUInit
    { initA :: Byte
    , initX :: Byte
    , initY :: Byte
    , initP :: Byte
    , initPC :: Maybe Addr
    }

instance Default CPUInit where
    def = CPUInit{ initA = 0x00
                 , initX = 0x00
                 , initY = 0x00
                 , initP = 0x00
                 , initPC = Nothing
                 }

cpu :: forall clk. (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu = cpu' def

cpu' :: forall clk. (Clock clk) => CPUInit -> CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu' CPUInit{..} CPUIn{..} = runRTL $ do

    -- State
    let (s0, pc0) = case initPC of
            Nothing -> (Init, 0x0000) -- PC to be filled in by Init
            Just pc -> (InitTest, pc)
    s <- newReg s0
    rPC <- newReg pc0

    rOp <- newReg 0x00
    let op = var rOp
        decoded@Decoded{..} = decode op
        Addressing{..} = dAddr
        size2 = addrImm .||. addrZP .||. addrIndirect
        _size3 = addrDirect
        size1 = addrNone -- bitNot $ size2 .||. size3

    rArgBuf <- newReg 0x00
    let argByte = cpuMemR
    let argWord = reg rArgBuf `appendS` argByte

    -- Registers
    rA <- newReg initA
    rX <- newReg initX
    rY <- newReg initY
    rSP <- newReg 0xFF
    let popTarget = 0x0100 .|. unsigned (reg rSP + 1)
        pushTarget = 0x0100 .|. unsigned (reg rSP)

    -- Flags
    fC <- newReg $ initP `testBit` 7
    fZ <- newReg $ initP `testBit` 6
    fI <- newReg $ initP `testBit` 5
    fD <- newReg $ initP `testBit` 4
    fB <- newReg $ initP `testBit` 3
    fV <- newReg $ initP `testBit` 1
    fN <- newReg $ initP `testBit` 0

    let flags = bitsToByte . Matrix.fromList . reverse $
                [ var fC
                , var fZ
                , var fI
                , var fD
                , var fB
                , high
                , var fV
                , var fN
                ]
        setFlags mtx = do
            let [c, z, i, d, b, _, v, n] = reverse . Matrix.toList . byteToBits $ mtx
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
                     , (addrImm, argByte)
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
                   , (high, argByte)
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
    let addr1 = mux (addrZP .||. addrIndirect)
                      (argWord + unsigned addrPreOffset,
                       unsigned $ argByte + addrPreOffset)
        run1 = do
            caseEx [ match dBranch $ \branch -> do
                          let (selector, target) = unpack branch
                          let branchFlag = branchFlags .!. selector
                              branchCond = branchFlag .==. target
                          WHEN branchCond $ do
                              rPC := reg rPC + signed argByte + 1
                          s := pureS Fetch1
                   , IF dJump $ do
                          CASE [ IF dReadMem $ do
                                      rNextA := argWord
                                      s := pureS FetchVector1
                               , OTHERWISE $ do
                                      rPC := argWord
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
                          rNextA := var rPC
                          s := pureS Fetch1
                   , IF dJSR $ do
                          rArgBuf := unsigned (reg rPC)
                          rSP := reg rSP - 2
                          rNextA := pushTarget
                          rNextW := enabledS $ unsigned (reg rPC `shiftR` 8)
                          rPC := addr1
                          s := pureS WaitPushAddr
                   , IF addrIndirect $ do
                          rNextA := addr1
                          s := pureS Indirect1
                   , IF dReadMem $ do
                          rNextA := addr1
                          s := pureS WaitRead
                   , IF dWriteMem $ do
                          rNextA := addr1
                          rNextW := enabledS res
                          s := pureS WaitWrite
                   ]

    let addrPostOffset = muxN [ (addrPostAddY, reg rY)
                              , (high, 0)
                              ]
    let addr2 = argWord + unsigned addrPostOffset
    let run2 = do
            caseEx [ IF (op `elemS` [0x24, 0x2C]) $ do -- BIT
                          fZ := (reg rA .&. argByte) .==. 0
                          fV := argByte `testABit` 6
                          fN := argByte `testABit` 7
                          rNextA := reg rPC
                          s := pureS Fetch1
                   , IF (addrIndirect .&&. reg s .==. pureS Indirect2) $ do
                          rNextA := addr2
                          CASE [ IF dWriteMem $ do
                                      rNextW := enabledS res
                                      s := pureS WaitWrite
                               , OTHERWISE $ do
                                      s := pureS WaitRead
                               ]
                   , OTHERWISE $ do
                          WHEN dUpdateFlags $ commitALUFlags
                          WHEN dWriteFlags $ setFlags argByte
                          WHEN dWriteA $ rA := res
                          WHEN dWriteX $ rX := res
                          WHEN dWriteY $ rY := res
                          CASE [ IF dWriteMem $ do
                                      rNextW := enabledS res
                                      s := pureS WaitWrite
                               , OTHERWISE $ do
                                      rNextA := reg rPC
                                      s := pureS Fetch1
                               ]
                   ]

    WHEN (bitNot cpuWait) $
      switch (reg s) $ \state -> case state of
          Init -> do
              rNextA := pureS resetVector
              s := pureS FetchVector1
          InitTest -> do
              rNextA := reg rPC
              s := pureS Fetch1
          FetchVector1 -> do
              rPC := unsigned cpuMemR
              let hi = reg rNextA .&. 0xFF00
                  lo = unsigned $ reg rNextA
              rNextA := hi .|. unsigned (lo + pureS (1 :: Byte))
              s := pureS FetchVector2
          FetchVector2 -> do
              let pc' = (reg rPC .&. 0xFF) .|. (unsigned cpuMemR `shiftL` 8)
              rPC := mux dRTS (pc', pc' + 1) -- BWAAAAH!
              rNextA := var rPC
              s := pureS Fetch1
          Fetch1 -> do
              rOp := cpuMemR
              caseEx [ IF (op .==. 0x00) $ do -- TODO: BRK
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
              WHEN size1 $ do
                  s := pureS Fetch1
              rPC := reg rPC + 1
              rNextA := var rPC
              s := pureS Fetch2
          Fetch2 -> do
              WHEN size2 $ do
                  run1
                  s := pureS Fetch1
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
              s := pureS Halt
          WaitPushAddr -> do
              rNextA := reg rNextA - 1
              rNextW := enabledS (reg rArgBuf)
              s := pureS WaitWrite
          WaitWrite -> do
              rNextA := reg rPC
              s := pureS Fetch1
          Halt -> do
              s := pureS Halt

    rNextW := disabledS

    let cpuMemA = var rNextA
        cpuMemW = var rNextW

    -- Debug view
    let cpuState = reg s
        cpuOp = op
        cpuArgBuf = reg rArgBuf
        cpuDecoded = decoded
    let cpuA = var rA
        cpuX = var rX
        cpuY = var rY
        cpuSP = var rSP
        cpuP = flags
        cpuPC = var rPC

    return (CPUOut{..}, CPUDebug{..})

resetVector :: Addr
resetVector = 0xFFFC

nmiVector :: Addr
nmiVector = 0xFFFA

irqVector :: Addr
irqVector = 0xFFFE
