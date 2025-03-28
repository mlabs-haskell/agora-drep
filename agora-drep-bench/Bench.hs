module Main (main) where

import Plutarch.Test.Bench (defaultMain)
import Test.Tasty (testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Benchmarks"
      []
