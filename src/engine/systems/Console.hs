{-

Console System that prints out a "name"

-}

module Console (attachConsole, console) where

import Engine
import Data.Map (Map, findWithDefault, insert, member)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)

consoleKey :: SystemKey
consoleKey = "ConSys"

consoleMsgDefault :: String
consoleMsgDefault = "Default Console Message!"

consoleDefault :: Component
consoleDefault = newConsole consoleMsgDefault

attachConsole :: Entity -> String -> Entity
attachConsole entity msg = insert consoleKey (oldComponentList ++ [newConsole msg]) entity
    where oldComponentList = findWithDefault [] consoleKey entity

newConsole :: String -> Component
newConsole msg = METADATA "Console System Instance" (COMPONENT (consoleSystem msg))

consoleSystem :: String -> System
consoleSystem msg entity = ([putStrLn msg], entity)

consoleIter :: [Component] -> System
consoleIter (x:xs) entity = ((fst iter) ++ (fst others), snd others)
    where 
        iter = (stripComponent x) entity
        others = consoleIter xs (snd iter)
consoleIter [] entity = ([], entity)

console :: System
console entity = consoleIter componentList entity
    where componentList = findWithDefault [consoleDefault] consoleKey entity