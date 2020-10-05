-- | An example of the Game Struct

module ExampleGame (exampleGame) where

import Engine

-- Entity Definitions
entities :: [Entity]
entities = [ addComponentToEntity ((newText "hello"), newEntity), addComponentToEntity ((newText "world"), newEntity)]



exampleGame :: Game
exampleGame = entities