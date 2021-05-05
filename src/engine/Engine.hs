-- | Module Definition for Laplace-Engine
module Engine (
    SystemKey,
    Entity,
    System,
    Component(..),
    Game,
    newGame,
    newEntity,
    run1Frame
) where

import Data.Map (Map, empty)
import qualified Data.Map as Map

{- 
    SystemKey: 
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String

{-
    Entity:
    Data Holder
    Holds the attached/acting Systems on the piece of data represented through Map's key
    Holds possible callstack of previous System Actions (Component)
-} 
type Entity = Map SystemKey [Component]

{-
    System:
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    *Laws:*
    1. Every System should have a SystemKey
    2. Every System should have a way to attach to the Entity or a Component Form
-}
type System = Entity -> ([IO ()], Entity)

{-
    Game:
    List of entities which can be filtered with system boolean map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
type Game = [Entity]

{-
    Component:
    Callstack of the Entity. Useful for previous frames, or a single element representing what
    to do for next frame. Remains abstract for the system using a self reference. A system
    itself doesn't represent data, but it's previous transformations can be used to calculate 
    against its own cardinality. A System has no state without a Component. An Entity carries 
    a Component.
-}
data Component = COMPONENT System

newGame :: Game
newGame = []

newEntity :: Entity
newEntity = empty

-- Hmmm... I wonder if the game itself represents an entity.
run1Frame :: [System] -> Game -> ([IO ()], Game)
run1Frame (x:xs) game = ((fst iter) ++ (fst others), snd others)
    where 
        iter = concatTplList (map x game)
        others = run1Frame xs (snd iter)

run1Frame [] game = ([], game)


concatTplList :: [([a], b)] -> ([a], [b])
concatTplList (cur:others) = (fst cur ++ (fst res), [snd cur] ++ (snd res))
    where res = concatTplList others
concatTplList [] = ([], [])

