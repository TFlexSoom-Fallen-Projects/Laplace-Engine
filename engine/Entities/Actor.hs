module Entities.Actor(
  actor
) where

import Engine(Entity, newEntityFromList)
import Systems.Console(newConsole, addMessage)

-- | Pre-made Actor Entity

actor :: Entity
actor = newEntityFromList [newConsole, addMessage "Hello World"]
