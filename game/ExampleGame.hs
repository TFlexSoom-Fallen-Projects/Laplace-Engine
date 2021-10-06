module ExampleGame (exampleGame) where

import SDK(
    newEntity,
    newEntityFromList,
    addEntity,
    Game,
    newGame)
import Systems.Console(enableConsole)
import Entities.Actor(actor)

{-

Tristan Hilbert
An example Game Built With Engine

-}
exampleGame :: Game
exampleGame = foldl (\ arg x -> x arg) newGame
    [ enableConsole 
    , addEntity actor ]