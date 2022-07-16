module Entities.Actor
  ( actor,
    actorCustom,
  )
where

import Core.Entity (Entity, newEntityFromList)
import Systems.Console (addMessage)

-- | Pre-made Actor Entity
actor :: Entity
actor = actorCustom "This is an Actor!"

actorCustom :: String -> Entity
actorCustom msg = newEntityFromList [addMessage msg]