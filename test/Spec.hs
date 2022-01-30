module Main (main) where

import SDK(runGame, runFrames)
import ExampleGame (exampleGame)

main :: IO ()
main = runGame exampleGame

-- main :: IO ()
-- main = hspec spec

-- spec :: Spec
-- spec = do
--   describe "Engine" Engine.SDKSpec.spec