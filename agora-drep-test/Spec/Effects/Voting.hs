module Spec.Effects.Voting (spec) where

import Agora.Effects.Voting (votingEffectValidator)
import Data.Text qualified as Text
import Plutarch.Internal.Term qualified as Term
import Spec.Effects.Voting.Context (certifyingContextSpec)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)

spec :: TestTree
spec =
  adjustOption extraOptions $
    testGroup
      "Voting Effect Validator"
      [ certifyingContextSpec script
      ]
  where
    script =
      either (error . Text.unpack) id $
        Term.compile (Term.Tracing Term.LogInfo Term.DoTracing) votingEffectValidator

    -- 100 tests is way too small for a property test to search for a counterexample
    extraOptions :: QuickCheckTests -> QuickCheckTests
    extraOptions = max 10_000
