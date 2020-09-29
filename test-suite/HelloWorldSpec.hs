module HelloWorldSpec (spec) where

import HelloWorld (helloWorld)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "helloWorld" $ do
        if "returns the unit value" $ do
            helloWorld `shouldBe` 1337
        
        prop "equals the unit value" $
            \ x -> helloWorld == x