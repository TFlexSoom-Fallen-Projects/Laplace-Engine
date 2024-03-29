module Entities.Triangle(
  triangle
) where

import Engine(Entity, newEntityFromList)
import Systems.Console(addMessage)
import Systems.GLFW(addGLFW)

-- | Pre-made Actor Entity

triangle :: Entity
triangle = newEntityFromList [addMessage "I am a Triangle", addGLFW]