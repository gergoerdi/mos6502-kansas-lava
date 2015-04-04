module MOS6502.Tests.Main (tests) where

import Distribution.TestSuite.QuickCheck as QC

import MOS6502.Tests
import MOS6502.Tests.Framework
import qualified MOS6502.Tests.AllSuiteA as AllSuiteA

tests :: IO [QC.Test]
tests = do
    allSuiteATest <- AllSuiteA.test
    return $ allSuiteATest : opTests
  where
    opTests = [ testProperty (testLabel test) (runTest test)
              | test <- allTests
              ]
