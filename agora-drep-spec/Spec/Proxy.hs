{- | Proxy script unit tests

@since 1.0.0
-}
module Spec.Proxy (spec) where

import Agora.Proxy (proxyScript)
import Data.Text qualified as Text
import Plutarch.Internal.Term qualified as Term
import Spec.Proxy.Context (mintingContextSpec, spendingContextSpec)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests)

{- | Proxy script unit tests

@since 1.0.0
-}
spec :: TestTree
spec =
  adjustOption extraOptions $
    testGroup
      "Proxy Validator"
      [ spendingContextSpec script
      , mintingContextSpec script
      ]
  where
    script =
      either (error . Text.unpack) id $
        Term.compile (Term.Tracing Term.LogInfo Term.DoTracing) proxyScript

    -- 100 tests is way too small for a property test to search for a counterexample
    extraOptions :: QuickCheckTests -> QuickCheckTests
    extraOptions = max 10_000
