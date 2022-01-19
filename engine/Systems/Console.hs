module Systems.Console (
    Console,
    addMessage,
) where

import Core.Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Core.SystemKey (SystemKey)
import Core.Component(Component(..))
import Core.Entity(Entity, addComponent)
import Core.System (
    Perspective(..),
    SingleInputSystem,
    SystemImpl(..),
    System(..)
    )

newtype Console = Console ()

instance System Console where
    getKey _ = consoleKey

    getImplementation _ = SINGLE console

    initContext _ = VALUE (toDyn ())

    initComponent _ = consoleDefault

consoleKey :: SystemKey
consoleKey = "ConSys"

addMessage :: String -> Entity -> Entity
addMessage msg = addComponent consoleKey (VALUE (toDyn msg))

-- Implementation

consoleDefault :: Component
consoleDefault = VALUE (toDyn "Hello World")

cast :: Component -> String
cast (VALUE c) = fromDyn c
cast _         = ""

console :: Perspective a => SingleInputSystem a
console modif = addIO modif ((putStrLn . (cast . getComponent)) modif)