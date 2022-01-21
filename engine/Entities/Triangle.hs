module Entities.Triangle(
  triangle
) where

import Core.Entity(Entity, newEntityFromList)
import Systems.GLFW(addGraphics)

-- | Pre-made Actor Entity

triangle :: Entity
triangle = newEntityFromList [addGraphics]

-- TODO Bug with empty entities