{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MOS6502.CPU where

import MOS6502.Opcodes
import MOS6502.Types
import MOS6502.Utils
import MOS6502.ALU

import Language.KansasLava
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix
import qualified Data.Sized.Matrix as Matrix
import Data.Bits
import Control.Monad ((<=<), void)

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

cpu :: forall clk. (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu CPUIn{..} = runRTL $ do
    -- State
    s <- newReg Init
    rOp <- newReg 0x00
    let (opAAA, opBBBCC) = unappendS (var rOp) :: (Signal clk U3, Signal clk U5)
        (opBBB, opCC) = unappendS opBBBCC :: (Signal clk U3, Signal clk U2)

    rArgBuf <- newReg 0x00
    let argByte = cpuMemR
    let argWord = reg rArgBuf `appendS` argByte

    -- Registers
    rA <- newReg 0x00
    rX <- newReg 0x00
    rY <- newReg 0x00
    rSP <- newReg 0xFF
    rPC <- newReg 0x0000 -- To be filled in by Init
    -- let popTarget = 0x0100 .|. unsigned (reg rSP + 1)
    --     pushTarget = 0x0100 .|. unsigned (reg rSP)

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

    rNextA <- newReg 0x0000
    rNextW <- newReg Nothing

    let binOp = bitwise opBBB
        binAddr = bitwise opAAA
    commitBinALU <- do
        let aluInC = reg fC
            aluInD = reg fD
        let (ALUOut{..}, a') = binaryALU binOp ALUIn{..} (reg rA) argByte
        return $ do
            CASE [ match aluOutC (fC :=) ]
            CASE [ match aluOutV (fV :=) ]
            fZ := aluOutZ
            fN := aluOutN
            rA := a'

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
                  isRTS = reg rOp .==. pureS 0x60
              rPC := mux isRTS (pc', pc' + 1) -- BWAAAAH!
              rNextA := var rPC
              s := pureS Fetch1
          Fetch1 -> do
              rOp := cpuMemR
              switch opCC $ \cc -> case cc of
                  0x1 -> do
                      rPC := reg rPC + 1
                      rNextA := var rPC
                      s := pureS Fetch2

              s := pureS Halt
          Fetch2 -> switch opCC $ \cc -> case cc of
              0x1 -> do
                  CASE [ IF (bitNot $ binIsLength2 binAddr) $ do
                              rArgBuf := cpuMemR
                              rPC := reg rPC + 1
                              rNextA := var rPC
                              s := pureS Fetch3
                       , IF (binOp .==. pureS STA) $ do
                              rNextW := enabledS (reg rA)
                              rNextA := switchS binAddr $ \addr -> case addr of
                                  ZP -> unsigned argByte
                                  ZP_X -> unsigned $ argByte + reg rX
                                  _ -> 0xDEAD
                              s := pureS WaitWrite
                       , OTHERWISE $ do
                              commitBinALU
                              rPC := reg rPC + 1
                              rNextA := var rPC
                              s := pureS Fetch2
                       ]
          Fetch3 -> switch opCC $ \cc -> case cc of
              0x1 -> do
                  rNextA := switchS binAddr $ \addr -> case addr of
                      Absolute -> argWord
                      Absolute_X -> argWord + unsigned (reg rX)
                      Absolute_Y -> argWord + unsigned (reg rY)
                      Indirect_X -> argWord + unsigned (reg rX)
                      Indirect_Y -> argWord
                      _ -> 0xDEAD
                  CASE [ IF (binIsIndirect binAddr) $ do
                              s := pureS Indirect1
                       , IF (binOp .==. pureS STA) $ do
                              rNextW := enabledS (reg rA)
                              s := pureS WaitWrite
                       , OTHERWISE $ do
                              s := pureS WaitRead
                       ]
          Indirect1 -> switch opCC $ \cc -> case cc of
              0x1 -> do
                  rArgBuf := cpuMemR
                  rNextA := reg rNextA + 1
                  s := pureS Indirect2
          Indirect2 -> switch opCC $ \cc -> case cc of
              0x1 -> do
                  rNextA := switchS binAddr $ \addr -> case addr of
                      Indirect_X -> argWord
                      Indirect_Y -> argWord + unsigned (reg rY)
                      _ -> 0xDEAD
                  CASE [ IF (binOp .==. pureS STA) $ do
                              rNextW := enabledS (reg rA)
                              s := pureS WaitWrite
                       , OTHERWISE $ do
                              s := pureS WaitRead
                       ]
          WaitRead -> switch opCC $ \cc -> case cc of
              0x1 -> do
                  commitBinALU
                  rNextA := reg rPC
                  s := pureS Fetch1
{-
          WaitPushAddr -> do
              rNextA := reg rNextA - 1
              rNextW := enabledS (reg rArgBuf)
              s := pureS WaitWrite
-}
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
        cpuOp = var rOp
        cpuArgBuf = reg rArgBuf
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
