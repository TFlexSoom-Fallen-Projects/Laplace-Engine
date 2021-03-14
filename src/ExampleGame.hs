-- | An example of the Game Struct

module ExampleGame (exampleGame, enabledSystems) where

import Engine
import Console

-- Entity Definitions
exampleGame :: Game
exampleGame = [(attachConsole newEntity "Hello World")]

enabledSystems :: [System]
enabledSystems = [console]