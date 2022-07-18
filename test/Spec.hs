module Main (main) where

import Engine.SDKSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec mainSpec

mainSpec :: Spec
mainSpec = do
  describe "Engine" Engine.SDKSpec.spec