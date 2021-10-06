module Engine (
    SystemKey,
    enableSystem,
    Entity,
    newEntity,
    newEntityFromList,
    addEntity,
    System,
    concatIO,
    Game,
    newGame,
    runFrame,
    dumpMetadata,
    runGame,
    Component(..),
    insertComponent,
    insertComponents,
    adjustDefaultComponent,
    adjustComponent
) where

import Data.Bifunctor(first, second)
import Data.Foldable(concatMap)
import Data.Map (Map, empty, findWithDefault, insert, member, fromList, adjust)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)

import Util (concatTplList)
import Data.Graph (components)

-- | Module Definition for Laplace-Engine

{- 
    SystemKey: 
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String

enableSystem :: String -> Game -> Game
enableSystem key = first (key :)

{-
    Entity:
    Data Holder
    Holds the attached/acting Systems on the piece of data represented through Map's key
    Holds possible callstack of previous System Actions (Component)
-}
type Entity = Map SystemKey [Component]

newEntity :: Entity
newEntity = empty

newEntityFromList :: [Entity -> Entity] -> Entity
newEntityFromList = foldl (\ arg x -> x arg) newEntity

addEntity :: Entity -> Game -> Game
addEntity entity = second (entity :)

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

-- TODO figure out how to remove derp
concatIO :: [Component] -> Entity -> [IO ()]
concatIO components entity = foldr derp [] results
    where
        derp result lst = lst ++ fst result
        results = map (`stripComponent` entity) components


-- Private
runSystem :: SystemKey -> Entity -> ([IO ()], Entity)
runSystem key entity = stripComponent (head components) entity
    where components = findWithDefault [] key entity

{-
    Game:
    List of entities which can be filtered with system boolean map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
type Game = ([SystemKey], [Entity])

newGame :: Game
newGame = ([], [])

runFrame :: Game -> ([IO ()], [Entity])
runFrame = uncurry runFrameCurried

dumpMetadata :: Game -> String
dumpMetadata = concatMap show

runGame :: Game -> IO ()
runGame game = do {
    (io, entities) <- return (runFrame game);
    foldr (>>) (pure ()) io;
    runGame (fst game, entities)
}

-- Private
runFrameCurried :: [SystemKey] -> [Entity] -> ([IO ()], [Entity])
runFrameCurried (x:xs) entities = first ( fst iter ++ ) others
    where
        iter = concatTplList (map (runSystem x) entities)
        others = runFrameCurried xs (snd iter)

runFrameCurried [] entities = ([], entities)

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

insertComponent :: SystemKey -> [Component] -> Entity -> Entity
insertComponent = insert

insertComponents :: [(SystemKey, [Component])] -> Entity -> Entity
insertComponents list entity = foldr (uncurry insertComponent) entity list

adjustDefaultComponent :: SystemKey -> [Component] -> [Component] -> Entity -> Entity
adjustDefaultComponent key addition defaultEntry entity = insert key newComponentList entity
    where
        newComponentList = oldComponentList ++ addition
        oldComponentList = findWithDefault defaultEntry key entity

adjustComponent :: SystemKey -> [Component] -> Entity -> Entity
adjustComponent key components = adjust (++ components) key

-- Private
stripComponent :: Component -> System
stripComponent ( METADATA _ a ) = stripComponent a
stripComponent ( COMPONENT a ) = a