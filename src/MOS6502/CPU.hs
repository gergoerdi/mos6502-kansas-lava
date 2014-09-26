{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module MOS6502.CPU where

import MOS6502.Types

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Data.Sized.Matrix (Matrix, (!))
import qualified Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix
import Data.Bits
import Data.Foldable (forM_)

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
    , cpuArgLo :: Signal clk Byte
    , cpuA :: Signal clk Byte
    , cpuX :: Signal clk Byte
    , cpuY :: Signal clk Byte
    , cpuP :: Signal clk Byte
    , cpuSP :: Signal clk Byte
    , cpuPC :: Signal clk Addr
    , cpuOp :: Signal clk Byte
    }

data State = Init
           | FetchVector1
           | FetchVector2
           | Fetch1
           | Fetch2
           | Fetch3
           | WaitMem
           | Halt
           deriving (Show, Eq, Enum, Bounded)
type StateSize = X8

instance Rep State where
    type W State = X3 -- W StateSize
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

cpu :: forall clk. (Clock clk) => CPUIn clk -> (CPUOut clk, CPUDebug clk)
cpu CPUIn{..} = runRTL $ do
    -- State
    s <- newReg Init
    rOp <- newReg 0x00
    rArgLo <- newReg 0x00

    -- Registers
    rA <- newReg 0x00
    rX <- newReg 0x00
    rY <- newReg 0x00
    rSP <- newReg 0x00
    rP <- newReg 0x00
    rPC <- newReg 0x0000 -- To be filled in by Init

    rNextA <- newReg 0x0000
    rNextW <- newReg Nothing

    WHEN (bitNot cpuWait) $
      switch (reg s)
      [ Init ==> do
             rNextA := pureS resetVector
             s := pureS FetchVector1
      , FetchVector1 ==> do
             rPC := unsigned cpuMemR
             rNextA := reg rNextA + 1
             s := pureS FetchVector2
      , FetchVector2 ==> do
             rPC := (reg rPC .&. 0xFF) .|. (unsigned cpuMemR `shiftL` 8)
             rNextA := var rPC
             s := pureS Fetch1
      , Fetch1 ==> do
             rOp := cpuMemR
             switch cpuMemR
               [ 0xA9 ==> do -- LDA #xx
                      rPC := reg rPC + 1
                      rNextA := var rPC
                      s := pureS Fetch2
               , 0x8D ==> do -- STA xxxx
                      rPC := reg rPC + 1
                      rNextA := var rPC
                      s := pureS Fetch2
               , oTHERWISE $ do
                      s := pureS Halt
               ]
      , Fetch2 ==> do
             switch (reg rOp)
               [ 0xA9 ==> do -- LDA #xx
                      rA := cpuMemR
                      rPC := reg rPC + 1
                      rNextA := var rPC
                      s := pureS Fetch1
               , 0x8D ==> do -- STA xxxx
                      rArgLo := cpuMemR
                      rPC := reg rPC + 1
                      rNextA := var rPC
                      s := pureS Fetch3
               ]
      , Fetch3 ==> do
             switch (reg rOp)
               [ 0x8D ==> do -- STA xxxx
                      rNextA := (unsigned cpuMemR `shiftL` 8) .|. unsigned (reg rArgLo)
                      rNextW := enabledS $ reg rA
                      rPC := reg rPC + 1
                      s := pureS WaitMem
               ]
      , WaitMem ==> do
             rNextW := disabledS
             rNextA := reg rPC
             s := pureS Fetch1
      ]

    let cpuMemA = var rNextA
        cpuMemW = var rNextW

    -- Debug view
    let cpuState = reg s
        cpuOp = reg rOp
        cpuArgLo = reg rArgLo
    let cpuA = reg rA
        cpuX = reg rX
        cpuY = reg rY
        cpuSP = reg rSP
        cpuP = reg rP
        cpuPC = reg rPC

    return (CPUOut{..}, CPUDebug{..})

resetVector :: Addr
resetVector = 0xFFFC

nmiVector :: Addr
nmiVector = 0xFFFA

irqVector :: Addr
irqVector = 0xFFFE

switch :: (Eq a, Rep a, sig ~ Signal c) => sig a -> [(Maybe a, RTL s c ())] -> RTL s c ()
switch r = CASE . map (uncurry $ maybe OTHERWISE toIF)
  where
    toIF x rtl = IF (r .==. pureS x) rtl

(==>) :: (Eq a, Rep a) => a -> RTL s c () -> (Maybe a, RTL s c ())
x ==> rtl = (Just x, rtl)

oTHERWISE :: (Eq a, Rep a) => RTL s c () -> (Maybe a, RTL s c ())
oTHERWISE rtl = (Nothing, rtl)
