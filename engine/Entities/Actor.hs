module Entities.Actor(
  actor
) where

-- | Pre-made Actor Entity

import Engine(newEntity, Entity)
import Systems.Console(attachConsole)

actor :: Entity
actor = attachConsole newEntity "Hello World"
