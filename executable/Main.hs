module Main (main) where

import Engine
import ExampleGame (exampleGame, enabledSystems)


passOffAction :: ([IO ()], Game) -> IO Game
passOffAction (action:actions, game) = do {
    action;
    passOffAction (actions, game)
}
passOffAction ([], game) = return game


loop :: Game -> IO ()
loop game = do {
    newGame <- passOffAction(run1Frame enabledSystems game);
    loop newGame
}

main :: IO ()
main = loop exampleGame