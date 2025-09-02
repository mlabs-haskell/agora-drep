{- | Test utilities

@since 1.0.0
-}
module Spec.Utils (TestConfig (..), mkTest, uncheckedApplyDataToScript) where

import Plutarch.Script (Script (Script))
import Plutarch.Test.Program (
  ScriptCase (ScriptCase),
  ScriptResult,
  testScript,
 )

import PlutusCore qualified as PLC
import PlutusLedgerApi.V3 (ScriptContext, ToData, toData)
import Test.Tasty (TestTree)
import UntypedPlutusCore (Program (Program), Term (Apply, Constant))

-- | @since 1.0.0
class TestConfig c where
  ownScript :: c -> Script
  testConfigFromScript :: Script -> c

{- | Build a tasty test case for a Plutarch script with given ScriptContext

@since 1.0.0
-}
mkTest :: (TestConfig c) => c -> String -> (c -> ScriptContext) -> ScriptResult -> TestTree
mkTest config testName toContext expectedResult =
  let script = ownScript config
      context = toContext config
      Script applied = uncheckedApplyDataToScript context script
   in testScript $ ScriptCase testName expectedResult applied applied

{- | Apply Plutus Data to a script

@since 1.0.0
-}
uncheckedApplyDataToScript :: (ToData argument) => argument -> Script -> Script
uncheckedApplyDataToScript argument (Script (Program () version unappliedTerm)) =
  Script
    . Program () version
    . Apply () unappliedTerm
    . Constant ()
    . PLC.someValue
    $ toData argument
