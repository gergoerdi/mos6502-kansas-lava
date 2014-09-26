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
