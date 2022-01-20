module Entities.Triangle(
  triangle
) where

import Core.Entity(Entity, newEntityFromList)
import Systems.Console(addMessage)

-- | Pre-made Actor Entity

triangle :: Entity
triangle = newEntityFromList [addMessage "I am a Triangle"]