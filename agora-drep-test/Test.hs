module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch.Test.Program ()
import Plutus.ContextBuilder ()
import Test.Tasty (defaultMain, testGroup)

import Agora.Drep.Test.Proxy qualified as Proxy

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain $
        testGroup
            "agora-drep"
            [ Proxy.spec
            ]
