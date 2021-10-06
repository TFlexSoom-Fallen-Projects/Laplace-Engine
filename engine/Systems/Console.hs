module Systems.Console (
    newConsole,
    addMessage,
    enableConsole
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
    adjustDefaultComponent)

-- | Console System that prints out a "name"

-- | Key Function Required By All Systems
consoleKey :: SystemKey
consoleKey = "ConSys"

-- Interface

newConsole :: Entity -> Entity
newConsole = insertComponent consoleKey consoleDefault

addMessage :: String -> Entity -> Entity
addMessage msg = adjustDefaultComponent consoleKey [METADATA "Console Message" (COMPONENT (console msg))] consoleDefault

enableConsole :: Game -> Game 
enableConsole = enableSystem consoleKey

-- Implementation

consoleDefault :: [Component]
consoleDefault = [METADATA "Console System Base" (COMPONENT consoleBase)]

consoleBase :: System
consoleBase entity = (concatIO (tail components) entity, entity)
    where components = findWithDefault [] consoleKey entity

console :: String -> System
console msg entity = ([putStrLn msg], entity)