module MOS6502.Utils where

import Language.KansasLava

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

switchS :: (Clock clk, Rep a, Eq a, Enum a, Bounded a)
        => Signal clk a -> (a -> Signal clk b) -> Signal clk b
switchS sig f = undefined
