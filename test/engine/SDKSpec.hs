module Engine.SDKSpec (spec) where

import Data.Map (Map, empty)
import qualified Data.Map as Map
import SDK (newEntity, newGame)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()

spec :: Spec
spec = do
  describe "newEntity" $ do
    it "returns an empty map" $ do
      newEntity `shouldBe` empty

  describe "newGame" $ do
    it "returns an empty list" $ do
      newGame `shouldBe` ([], [])
