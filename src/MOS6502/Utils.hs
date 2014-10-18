module MOS6502.Utils where

import Language.KansasLava

memoryMapping :: (Clock clk, Rep a)
              => Signal clk a
              -> [(Signal clk Bool, Signal clk a)]
              -> Signal clk a
memoryMapping = foldr (\(sel, v) sig -> mux (delay sel) (sig, v))

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

muxN :: (Clock clk, Rep a)
     => [(Signal clk Bool, Signal clk a)] -> Signal clk a
muxN = foldr (\(b, y) sig -> mux b (sig, y)) undefinedS
