module Engine (
    SystemKey,
    Entity,
    newEntity,
    newEntityFromList,
    System,
    attachSystem,
    attachSystems,
    Game,
    newGame,
    run1Frame,
    dumpMetadata,
    Component(..),

) where

import Data.Bifunctor(first)
import Data.Foldable(concatMap)
import Data.Map (Map, empty, findWithDefault, insert, member, fromList)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)

import Util (concatTplList)

-- | Module Definition for Laplace-Engine

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

newEntity :: Entity
newEntity = empty

newEntityFromList :: [(SystemKey, Component)] -> Entity
newEntityFromList list = attachSystems list newEntity

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

attachSystem :: SystemKey -> Component -> Entity -> Entity
attachSystem key comp entity = insert key newComponentList entity
    where 
        newComponentList = oldComponentList ++ [comp]
        oldComponentList = findWithDefault [] key entity

attachSystems :: [(SystemKey, Component)] -> Entity -> Entity
attachSystems list entity = foldr (uncurry attachSystem) entity list 

-- Private
iterSysComps :: [Component] -> System
iterSysComps (x:xs) entity = first ( fst iter ++ ) others
    where 
        iter = stripComponent x entity
        others = iterSysComps xs (snd iter)
iterSysComps [] entity = ([], entity)

-- Private
runSystem :: SystemKey -> System
runSystem key entity = iterSysComps componentList entity
    where componentList = findWithDefault [] key entity

{-
    Game:
    List of entities which can be filtered with system boolean map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
type Game = [Entity]

newGame :: Game
newGame = []

run1Frame :: [SystemKey] -> Game -> ([IO ()], Game)
run1Frame (x:xs) game = first ( fst iter ++ ) others
    where
        iter = concatTplList (map (runSystem x) game)
        others = run1Frame xs (snd iter)

run1Frame [] game = ([], game)

dumpMetadata :: Game -> [Char]
dumpMetadata = concatMap show

{-
    Component:
    Callstack of the Entity. Useful for previous frames, or a single element representing what
    to do for next frame. Remains abstract for the system using a self reference. A system
    itself doesn't represent data, but it's previous transformations can be used to calculate 
    against its own cardinality. A System has no state without a Component. An Entity carries 
    a Component.
-}
data Component = METADATA String Component | COMPONENT System

instance Show Component where
    show ( METADATA msg a ) = (++) taggedMetadata (show a)
        where taggedMetadata = (++) ((++) "x" msg) ">"
    show ( COMPONENT a ) = "{System}"

instance Eq Component where
    (==) ( METADATA msg a ) (METADATA msg1 b ) = (&&) ((==) msg msg1) ((==) a b)
    (==) ( COMPONENT _ ) (COMPONENT _ ) = True
    (==) _ _ = False

-- Private
stripComponent :: Component -> System
stripComponent ( METADATA _ a ) = stripComponent a
stripComponent ( COMPONENT a ) = a