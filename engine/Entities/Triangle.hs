module Entities.Triangle
  ( triangle,
  )
where

import Core.Entity (Entity, newEntityFromList)

-- | Pre-made Actor Entity
triangle :: Entity
triangle = newEntityFromList []

-- TODO Bug with empty entities