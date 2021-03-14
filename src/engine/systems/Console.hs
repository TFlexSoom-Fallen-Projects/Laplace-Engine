{-

Console System that prints out a "name"

-}

module Console (attachConsole, console) where

import Engine
import Data.Map (Map, findWithDefault, insert, member)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)

consoleKey :: SystemKey
consoleKey = "Console System"

consoleMsgDefault :: String
consoleMsgDefault = "Some Entity"

consoleDefault :: Component
consoleDefault = newConsole consoleMsgDefault

attachConsole :: Entity -> String -> Entity
attachConsole entity msg = insert consoleKey (oldConsoleList ++ [newConsole msg]) entity
    where oldConsoleList = findWithDefault [] consoleKey entity

newConsole :: String -> Component
newConsole msg = Tail (consoleSystem msg)

consoleSystem :: String -> System
consoleSystem msg entity = entity

consoleIter :: [Component] -> Entity -> Entity
consoleIter ((Tail system):xs) entity = consoleIter xs (system entity)
consoleIter [] entity = entity

console :: System
console entity = consoleIter componentList entity
    where componentList = findWithDefault [consoleDefault] consoleKey entity