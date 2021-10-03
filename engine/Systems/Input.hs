module Systems.Input (
    inputKey,
    newInput
) where

import Engine ( Component(..), System, Entity, SystemKey )

-- | Input Systems for Keys, Joysticks, Buttons, etc.

data InputType = BUTTON | RANGE_1D | RANGE_2D | RANGE_3D | ENCODED
    deriving Show

inputKey :: SystemKey
inputKey = "inputSys"

newInput :: InputType -> Component
newInput t = METADATA "Console System Instance" (COMPONENT (input t))

input :: InputType -> System
input t entity = ([print t], entity)