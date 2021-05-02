module Main (main) where

import Engine
import EngineIO (performActions)
import ExampleGame (exampleGame, enabledSystems)


passOffAction :: ([Action], Game) -> Game
passOffAction (actionList, game) = performActions actionList >> game

loop :: Game -> IO ()
loop game = loop newGame
    where newGame = passOffAction(run1Frame enabledSystems game)

main :: IO ()
main = loop exampleGame