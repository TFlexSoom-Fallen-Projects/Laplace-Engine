module Main (main) where

import Test.Hspec
import qualified Engine.EngineSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Engine" Engine.EngineSpec.spec