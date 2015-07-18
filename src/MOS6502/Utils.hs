{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module MOS6502.Utils where

import Language.KansasLava
import Language.KansasLava.Signal
import Data.Sized.Arith
import Data.Sized.Ix

memoryMapping :: (Clock clk, Rep a)
              => [(Signal clk Bool, Signal clk a)]
              -> Signal clk a
memoryMapping = foldr (\(sel, v) sig -> mux (delay sel) (sig, v)) undefinedS

-- | Non-overlapping case
caseEx :: [Cond s c] -> RTL s c ()
caseEx = foldr (\c rtl -> CASE [c, OTHERWISE rtl]) (return ())

switch :: (Clock clk, Rep a, Eq a, Enum a, Bounded a)
       => Signal clk a -> (a -> RTL s clk ()) -> RTL s clk ()
switch sig f =
    CASE
      [ IF (sig .==. pureS x) act
      | x <- [minBound..maxBound]
      , let act = f x
      ]

elemS' :: (Clock clk, Rep a, Eq a) => Signal clk a -> [Signal clk a] -> Signal clk Bool
sig `elemS'` sigs = foldr (\x b -> sig .==. x .||. b) low sigs

elemS :: (Clock clk, Rep a, Eq a) => Signal clk a -> [a] -> Signal clk Bool
sig `elemS` xs = sig `elemS'` map pureS xs

switchS :: (Clock clk, Rep a, Rep b, Eq a)
        => Signal clk a -> [(a, Signal clk b)] -> Signal clk b
switchS sig = foldr (\(x,y) sig' -> mux (sig .==. pureS x) (sig', y)) undefinedS

muxN :: (Rep a)
     => [(Signal clk Bool, Signal clk a)] -> Signal clk a
muxN = foldr (\(b, y) sig -> mux b (sig, y)) undefinedS

muxN2 :: forall clk a b. (Rep a, Rep b)
      => [(Signal clk Bool, (Signal clk a, Signal clk b))] -> (Signal clk a, Signal clk b)
muxN2 = unpack . muxN . map (\(sel, xy) -> (sel, pack xy :: Signal clk (a, b)))

muxMatch :: (Rep a) => Signal clk (Enabled a) -> (Signal clk a -> Signal clk b) -> (Signal clk Bool, Signal clk b)
muxMatch s f = (isEnabled s, f (enabledVal s))

fallingEdge :: (Clock clk) => Signal clk Bool -> Signal clk Bool
fallingEdge sig = runRTL $ do
    prev <- newReg False
    prev := sig
    return $ reg prev .&&. bitNot sig

risingEdge :: (Clock clk) => Signal clk Bool -> Signal clk Bool
risingEdge sig = runRTL $ do
    prev <- newReg True
    prev := sig
    return $ bitNot (reg prev) .&&. sig

(.=<<.) :: (Rep a, Rep b)
        => (forall clk. Signal clk a -> Signal clk (Enabled b))
        -> Signal clk (Enabled a) -> Signal clk (Enabled b)
f .=<<. s = packEnabled (isEnabled s .&&. isEnabled s') $ enabledVal s'
  where
    s' = f (enabledVal s)

(&*) :: forall a b n n'. (Size n, Size n', Size (ADD n' n), n ~ SUB (ADD n' n) n', n' ~ SUB (ADD n' n) n)
     => [(a -> b, BitPat n)] -> [(a, BitPat n')] -> [(b, BitPat (ADD n' n))]
mks &* args = [(mk arg, argRep & mkRep) | (mk, mkRep) <- mks, (arg, argRep) <- args]
