module ExampleGame (exampleGame) where

import SDK(
    Creatable(..),
    newEntityFromList,
    addEntity,
    Game,
    )
import Systems.Console(enableConsole)
import Entities.Triangle(triangle)

{-

Tristan Hilbert
An example Game Built With Engine

-}
exampleGame :: Game
exampleGame = foldl (\ arg x -> x arg) (new :: Game)
    [ 
          enableConsole 
        , addEntity triangle
    ]