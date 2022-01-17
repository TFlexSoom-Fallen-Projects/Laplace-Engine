module ExampleGame (exampleGame) where

import SDK(
    Creatable(..),
    newEntityFromList,
    addEntity,
    Game,
    )
import Systems.Console(enableConsole)
import Entities.Actor(actor, actorCustom)

{-

Tristan Hilbert
An example Game Built With Engine

-}
exampleGame :: Game
exampleGame = foldl (\ arg x -> x arg) (new :: Game)
    [ 
          enableConsole 
        , addEntity (actorCustom "This is an Entity 1") 
        , addEntity (actorCustom "This Is an Entity 2")
        , addEntity (actorCustom "This Is an Entity 3")
    ]