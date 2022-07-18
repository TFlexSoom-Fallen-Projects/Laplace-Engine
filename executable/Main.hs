module Main (main) where

import ExampleGame (exampleGame)
import SDK (runFrames, runGame)

-- main :: IO ()
-- main = runGame exampleGame

main :: IO ()
main = runFrames 20 exampleGame