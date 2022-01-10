module Systems.Input (
    -- newInput,
    -- addInput,
    -- enableInput
) where

import Data.Map(findWithDefault)
import Core.Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Engine (
    SystemKey,
    enableSystem,
    Entity,
    System,
    SystemOutput(..),
    Game,
    Component(..))

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