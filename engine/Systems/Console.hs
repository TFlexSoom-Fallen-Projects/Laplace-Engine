module Systems.Console (
    newConsole,
    addMessage,
    enableConsole
) where

import Data.Map(findWithDefault)
import Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Engine (
    SystemKey,
    enableSystem,
    Entity(..),
    newEntity,
    System,
    Game,
    Component(..),
    SystemOutput(..),
    replaceComponent)

-- | Console System that prints out a "name"

-- | Key Function Required By All Systems
consoleKey :: SystemKey
consoleKey = "ConSys"

-- Interface

newConsole :: Entity -> Entity
newConsole = replaceComponent consoleKey consoleDefault

addMessage :: String -> Entity -> Entity
addMessage msg = replaceComponent consoleKey (VALUE (toDyn msg))

enableConsole :: Game -> Game
enableConsole =  enableSystem consoleKey console

-- Implementation

consoleDefault :: Component
consoleDefault = VALUE (toDyn "Hello World")

cast :: Component -> String
cast (VALUE c) = fromDyn c
cast _         = ""

console :: System
console comp = SystemOutput {
    io = [putStrLn (cast comp)],
    component = comp,
    new = []
}