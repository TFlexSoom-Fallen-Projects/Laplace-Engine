{-
    =__System:__
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    ==__Laws:__
    1. Every System should have a SystemKey
    2. (len input) == (len output)
-}
module Core.System (
    -- For Engine Use Only
    Priority,
    SharingKey,
    Perspective(..),
    SingleInputSystem,
    MultiInputSystem,
    SystemImpl(..),
    System(..)
) where

import Core.SystemKey (SystemKey)
import Core.Component (Component(..))

type Priority = Maybe Int
type SharingKey = String
-- TODO Keys would be better as 64 bit integers than strings

class Perspective a where
    -- Modifications to Domain
    getComponent :: a -> Component
    alterComponent :: a -> (Component -> Maybe Component) -> a
    setComponent :: a -> Component -> a

    -- Modifications to Context
    getContext :: a -> Component
    alterContext :: a -> (Component -> Maybe Component) -> a
    setContext :: a -> Component -> a

    -- Modifications to Domain's Owner
    setToDelete :: a -> a

    -- Sharing (is caring ;) )
    share :: a -> SharingKey -> Component -> a
    receive :: a -> SharingKey -> Component

type SingleInputSystem a = a -> a
type MultiInputSystem a = [a] -> [Maybe a]

data SystemImpl a = 
     SINGLE (SingleInputSystem a)
    --              V Can be any Ord instance
    | BATCH  (a -> Priority) (MultiInputSystem a)
    | ALL                    (MultiInputSystem a)

class System a where
    getKey :: a -> SystemKey
    getImplementation :: Perspective b => a -> SystemImpl b
    initContext :: a -> Component 
