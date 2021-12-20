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
    Entity,
    System,
    Game,
    Component(..),
    SystemOutput(..),
    insertComponent,
    adjustDefaultComponent)

-- | Console System that prints out a "name"

-- | Key Function Required By All Systems
consoleKey :: SystemKey
consoleKey = "ConSys"

-- Interface

newConsole :: Entity -> Entity
newConsole = insertComponent consoleKey consoleDefault

addMessage :: String -> Entity -> Entity
addMessage msg = adjustDefaultComponent consoleKey [VALUE (toDyn msg)] consoleDefault

enableConsole :: Game -> Game
enableConsole =  enableSystem consoleKey console

-- Implementation

consoleDefault :: [Component]
consoleDefault = [VALUE (toDyn "Hello World")]

concatValues :: [Component] -> String
concatValues = concatMap cast
    where 
        cast (VALUE c) = fromDyn c
        cast _ = ""

console :: System
console comps = SystemOutput {
    io = [putStrLn (concatValues comps)],
    entity = comps,
    new = []
}