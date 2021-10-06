module Systems.Input (
    newInput,
    addInput,
    enableInput
) where

import Data.Map(findWithDefault)
import Engine (
    SystemKey,
    enableSystem,
    Entity,
    System,
    concatIO,
    Game,
    Component(..),
    insertComponent,
    insertComponents,
    adjustDefaultComponent,
    adjustComponent)

-- | Input Systems for Keys, Joysticks, Buttons, etc.

-- | Key Function Required By All Systems
inputKey :: SystemKey
inputKey = "inputSys"

-- Interface

data InputType = BUTTON | RANGE_1D | RANGE_2D | RANGE_3D | ENCODED
    deriving Show

newInput :: Entity -> Entity
newInput = insertComponent inputKey inputDefault

addInput :: InputType -> Entity -> Entity 
addInput t = adjustDefaultComponent inputKey [METADATA "Input Listener" (COMPONENT (input t))] inputDefault

enableInput :: Game -> Game 
enableInput = enableSystem inputKey

-- Implmentation

inputDefault :: [Component]
inputDefault = [METADATA "Input System Base" (COMPONENT inputBase)]

inputBase :: System
inputBase entity = (concatIO (tail components) entity, entity)
    where components = findWithDefault [] inputKey entity 

input :: InputType -> System
input BUTTON entity = ([putStrLn "Mouse Input Used"], entity)
input _      entity = ([putStrLn "Unknown Input Used"], entity)