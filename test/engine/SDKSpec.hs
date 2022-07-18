module Engine.SDKSpec (spec) where

import Core.EngineV1 (runFrame)
import SDK (GameImpl, gameV1)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()

spec :: Spec
spec = do
  describe "gameV1" $ do
    it "returns empty IO lists when empty" $ do
      length (fst (runFrame gameV1)) `shouldBe` 0
