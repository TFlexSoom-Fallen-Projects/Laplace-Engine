module Main (main) where

import Test.Hspec
import qualified Engine.SDKSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Engine" Engine.SDKSpec.spec