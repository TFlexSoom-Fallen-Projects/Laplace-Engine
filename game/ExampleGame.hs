module ExampleGame (exampleGame, enabledSystems) where

import Engine(Game, System)
import Entities.Actor(actor)
import Systems.Console(console)

{-

Tristan Hilbert
An example Game Built With Engine

-}

-- Entity Definitions
exampleGame :: Game
exampleGame = [actor]

enabledSystems :: [System]
enabledSystems = [console]