module Main (main) where

import Engine
import ExampleGame (exampleGame, enabledSystems)


complete :: ([System] -> Game) -> IO()
complete _ = putStrLn "Completed Game!"

main :: IO()
main = do {
    putStrLn "Hello World!";
    complete (run enabledSystems exampleGame)
}