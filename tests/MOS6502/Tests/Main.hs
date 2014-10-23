module MOS6502.Tests.Main (tests) where

import Distribution.TestSuite.QuickCheck as QC

import MOS6502.Tests
import MOS6502.Tests.Framework

tests :: IO [QC.Test]
tests = return [ testProperty (testLabel test) (runTest test) | test <- allTests ]
