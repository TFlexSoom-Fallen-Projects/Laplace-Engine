module Entities.Actor(
  actor
) where

import Engine(newEntity, Entity)
import Systems.Console(attachConsole)

-- | Pre-made Actor Entity

actor :: Entity
actor = attachConsole newEntity "Hello World"
