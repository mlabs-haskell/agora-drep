module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Spec.Proxy qualified as Proxy
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "agora-drep-onchain"
      [ Proxy.spec
      ]
