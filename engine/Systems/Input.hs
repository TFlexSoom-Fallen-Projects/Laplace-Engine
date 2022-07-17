module Systems.Input
  (
  )
where

-- newInput,
-- addInput,
-- enableInput

import Core.Component (Component (..))
import Core.Dynamic (Dynamic, DynamicHolder (..), DynamicallyAware (..))
import Core.Entity (Entity, addComponent)
import Core.Game (Game)
import Core.System
  ( Priority,
    SharingKey,
    System (..),
  )
import Core.SystemKey (SystemKey)

-- | Input Systems for Keys, Joysticks, Buttons, etc.

-- | Key Function Required By All Systems
inputKey :: SystemKey
inputKey = "inputSys"

-- Interface

-- TODO make instance of DynamicallyAware
data InputType = BUTTON | RANGE_1D | RANGE_2D | RANGE_3D | ENCODED
  deriving (Show)

-- newInput :: Entity -> Entity
-- newInput = insertComponent inputKey inputDefault

-- -- TODO FIX
-- addInput :: InputType -> Entity -> Entity
-- addInput t = adjustDefaultComponent inputKey [] inputDefault

-- enableInput :: Game -> Game
-- enableInput = enableSystem inputKey input

-- -- Implmentation

-- inputDefault :: [Component]
-- inputDefault = [VALUE (toDyn "Hello World")]

-- input :: System
-- input comps = SystemOutput {
--     io = [putStrLn "Mouse Input Used!"],
--     entity = comps,
--     new = []
-- }