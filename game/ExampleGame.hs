module ExampleGame (exampleGame) where

import SDK(
    Creatable(..),
    newEntityFromList,
    addEntity,
    Game(..),
    game,
    addToGame,
    GameImpl,
    )
import Entities.Triangle(triangle)
import Systems.Console(console)
import Systems.GLFW(glfw)

{-

Tristan Hilbert
An example Game Built With Engine

-}
exampleGame :: GameImpl
exampleGame = addToGame [
          enable glfw
        , addEntity triangle
    ] game 

-- exampleGame = foldl (\ arg x -> x arg) game
--     [ 
--           enableConsole 
--         , addEntity triangle
--     ]