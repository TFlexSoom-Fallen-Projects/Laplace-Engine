module Entities.Actor(
  actor,
  actorCustom
) where

import Engine(Entity, newEntityFromList)
import Systems.Console(newConsole, addMessage)

-- | Pre-made Actor Entity

actor :: Entity
actor = actorCustom "This is an Actor!"

actorCustom :: String -> Entity
actorCustom msg = newEntityFromList [newConsole, addMessage msg]