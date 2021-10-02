module Systems.Console (
    attachConsole, 
    console
) where

import Engine
    ( Component(..), System, Entity, SystemKey, stripComponent )
import Data.Map (Map, findWithDefault, insert, member)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)
import Data.Bifunctor(first)

-- | Console System that prints out a "name"

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
consoleIter (x:xs) entity = first ( fst iter ++ ) others
    where 
        iter = stripComponent x entity
        others = consoleIter xs (snd iter)
consoleIter [] entity = ([], entity)

console :: System
console entity = consoleIter componentList entity
    where componentList = findWithDefault [consoleDefault] consoleKey entity