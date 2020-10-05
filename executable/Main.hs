module Main (main) where

import HelloWorld (helloWorld)
import Engine (engine)
import ExampleGame (exampleGame)

main :: IO()
main = do {
    print helloWorld;
    engine exampleGame
}