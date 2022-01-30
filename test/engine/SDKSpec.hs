module Engine.SDKSpec (spec) where

import SDK (newGame, newEntity)
import Data.Map (Map, empty)
import qualified Data.Map as Map
import Test.Hspec ( describe, it, shouldBe, Spec )
import Test.QuickCheck ()

spec :: Spec
spec = do
    describe "newEntity" $ do
        it "returns an empty map" $ do
            newEntity `shouldBe` empty
    
    describe "newGame" $ do
        it "returns an empty list" $ do
            newGame `shouldBe` ([], [])

