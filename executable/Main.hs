module Main (main) where

import Engine
import ExampleGame (exampleGame, enabledSystems)


passOffAction :: ([IO ()], Game) -> IO Game
passOffAction ((action:actions), game) = do {
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
main = do {
    game1 <- passOffAction(run1Frame enabledSystems exampleGame);
    do {
        game2 <- passOffAction(run1Frame enabledSystems game1);
        do {
            game3 <- passOffAction(run1Frame enabledSystems game2);
            do {
                putStrLn (dumpMetadata game3)
            }
        }
    }
}