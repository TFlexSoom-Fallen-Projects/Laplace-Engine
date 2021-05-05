module Engine.EngineSpec (spec) where

import Engine (newGame, newEntity, run1Frame)
import Console
import Data.Map (Map, empty)
import qualified Data.Map as Map
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "newEntity" $ do
        it "returns an empty map" $ do
            newEntity `shouldBe` empty
        
        it "equals the unit value" $
            property $ \x -> newEntity == x
    
    describe "newGame" $ do
        it "returns an empty list" $ do
            newGame `shouldBe` []
        
        it "equals the unit value" $
            property $ \x -> newGame == x

