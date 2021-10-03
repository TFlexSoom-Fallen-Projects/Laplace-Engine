module Entities.Actor(
  actor
) where

import Engine(Entity, newEntityFromList)
import Systems.Console(consoleKey, newConsole)

-- | Pre-made Actor Entity

actor :: Entity
actor = newEntityFromList [(consoleKey, newConsole "Hello World")]
