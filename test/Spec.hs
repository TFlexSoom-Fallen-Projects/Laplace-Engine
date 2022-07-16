module Main (main) where

import ExampleGame (exampleGame)
import SDK (runFrames, runGame)

main :: IO ()
main = runGame exampleGame

-- main :: IO ()
-- main = hspec spec

-- spec :: Spec
-- spec = do
--   describe "Engine" Engine.SDKSpec.spec