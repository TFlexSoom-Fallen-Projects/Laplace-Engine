module Main (main) where

import SDK(runGame, runFrames)
import ExampleGame (exampleGame)

main :: IO ()
main = runGame exampleGame

-- main :: IO ()
-- main = runFrames 3 exampleGame