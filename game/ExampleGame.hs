module ExampleGame (exampleGame, enabledSystems) where

import Engine(Game, SystemKey)
import Entities.Actor(actor)
import Systems.Console(consoleKey)

{-

Tristan Hilbert
An example Game Built With Engine

-}

-- Entity Definitions
exampleGame :: Game
exampleGame = [actor]

enabledSystems :: [SystemKey]
enabledSystems = [consoleKey]