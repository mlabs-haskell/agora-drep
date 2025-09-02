{- | Voting Effect script unit tests

@since 1.0.0
-}
module Spec.Effect.Voting (spec) where

import Agora.Effect.Voting (votingEffectScript)
import Data.Text qualified as Text
import Plutarch.Internal.Term qualified as Term
import Spec.Effect.Voting.Context (certifyingContextSpec, spendingContextSpec, votingContextSpec)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)

{- | Voting Effect script unit tests

@since 1.0.0
-}
spec :: TestTree
spec =
  adjustOption extraOptions $
    testGroup
      "Voting Effect Validator"
      [ certifyingContextSpec script
      , votingContextSpec script
      , spendingContextSpec script
      ]
  where
    script =
      either (error . Text.unpack) id $
        Term.compile (Term.Tracing Term.LogInfo Term.DoTracing) votingEffectScript

    -- 100 tests is way too small for a property test to search for a counterexample
    extraOptions :: QuickCheckTests -> QuickCheckTests
    extraOptions = max 10_000
