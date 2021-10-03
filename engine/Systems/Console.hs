module Systems.Console (
    consoleKey,
    newConsole
) where

import Engine
    ( Component(..), System, Entity, SystemKey )

-- | Console System that prints out a "name"

consoleKey :: SystemKey
consoleKey = "ConSys"

newConsole :: String -> Component
newConsole msg = METADATA "Console System Instance" (COMPONENT (console msg))

console :: String -> System
console msg entity = ([putStrLn msg], entity)

