module Systems.Input (
    -- newInput,
    -- addInput,
    -- enableInput
) where

import Core.Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Core.SystemKey (SystemKey)
import Core.Component(Component(..))
import Core.Entity(Entity, addComponent)
import Core.Game(Game)
import Core.System (
    Priority,
    SharingKey,
    Perspective(..),
    SingleInputSystem,
    MultiInputSystem,
    SystemImpl(..),
    System(..)
    )

-- | Input Systems for Keys, Joysticks, Buttons, etc.

-- | Key Function Required By All Systems
inputKey :: SystemKey
inputKey = "inputSys"

-- Interface

-- TODO make instance of DynamicallyAware
data InputType = BUTTON | RANGE_1D | RANGE_2D | RANGE_3D | ENCODED
    deriving Show

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