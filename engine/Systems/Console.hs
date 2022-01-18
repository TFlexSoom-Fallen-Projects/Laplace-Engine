module Systems.Console (
    newConsole,
    addMessage,
    enableConsole
) where

import qualified Data.Map as Map
import Core.Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Engine (
    Component(..),
    SingleInputSystem,
    System(..),
    SystemKey,
    ShareMap,
    SystemInput(..),
    SystemOutput(..),
    Modification(..),
    EngineJob(..),
    enableSystem,
    Entity(..),
    addComponent,
    Game,
    )

-- | Console System that prints out a "name"

-- | Key Function Required By All Systems
consoleKey :: SystemKey
consoleKey = "ConSys"

-- Interface

newConsole :: Entity -> Entity
newConsole = addComponent consoleKey consoleDefault

addMessage :: String -> Entity -> Entity
addMessage msg = addComponent consoleKey (VALUE (toDyn msg))

enableConsole :: Game -> Game
enableConsole =  enableSystem consoleKey console

-- Implementation

consoleDefault :: Component
consoleDefault = VALUE (toDyn "Hello World")

cast :: Component -> String
cast (VALUE c) = fromDyn c
cast _         = ""

console :: System 
console = SINGLE consoleImpl

consoleImpl :: SingleInputSystem
consoleImpl SystemInput {component=comp} = SystemOutput {
    modification = Modification {
            modified = comp,
            delete = False,
            newShares = Map.empty
        },
    job = EngineJob {
        io = [putStrLn (cast comp)],
        added = []
        }
}