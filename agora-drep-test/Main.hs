{- | Agora DRep tests

@since 1.0.0
-}
module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Spec.Effect.Voting qualified as VotingEffect
import Spec.Proxy qualified as Proxy
import Test.Tasty (defaultMain, testGroup)

{- | Agora DRep tests

@since 1.0.0
-}
main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "agora-drep-onchain"
      [ Proxy.spec
      , VotingEffect.spec
      ]
