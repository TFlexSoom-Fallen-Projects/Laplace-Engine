module Engine (
    SystemKey,
    enableSystem,
    Entity,
    newEntity,
    newEntityFromList,
    addEntity,
    System,
    SystemOutput(..),
    Game,
    newGame,
    runFrame,
    runGame,
    Component(..),
    insertComponent,
    insertComponents,
    adjustDefaultComponent,
    adjustComponent
) where

import Data.Bifunctor(first, second, bimap)
import Data.Foldable(concatMap)
import Data.Map (Map, empty, findWithDefault, insert, member, fromList, adjust, foldrWithKey)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)

import Dynamic (Dynamic, DynamicallyAware, DynamicHolder)

-- | Module Definition for Laplace-Engine
-- TODO should have I have Runtime Defined Modules?

{- 
    SystemKey: 
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String

enableSystem :: String -> System -> Game -> Game
enableSystem key sys g = replaceSystems g newSystems
    where newSystems = insert key sys (systems g)

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
addEntity entity g = replaceEntities g (entities g ++ [entity])

{-
    System:
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    *Laws:*
    1. Every System should have a SystemKey
-}
data SystemOutput = SystemOutput {
    io :: [IO ()],
    entity :: [Component],
    new :: [Entity]
}
type System = [Component] -> SystemOutput

{-
    Game:
    List of entities which can be filtered with system boolean map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
data Game = Game {
    systems :: Map SystemKey System,
    entities :: [Entity]
}

newGame :: Game
newGame = Game {
    systems=empty,
    entities=[]
}

replaceSystems :: Game -> Map SystemKey System -> Game
replaceSystems g sys = Game {
    systems = sys,
    entities = entities g
}

replaceEntities :: Game -> [Entity] -> Game
replaceEntities g e = Game {
    systems = systems g,
    entities = e
}

runGame :: Game -> IO ()
runGame game = do {
    (io, modifiedGame) <- return (runFrame game);
    foldr (>>) (pure ()) io;
    runGame modifiedGame
}

runFrame :: Game -> ([IO ()], Game)
runFrame g = second (replaceEntities g) result
    where result = foldrWithKey runSystemOnEntities ([], entities g) (systems g)

-- Private

runSystemOnEntities :: SystemKey -> System -> ([IO ()], [Entity]) -> ([IO ()], [Entity])
runSystemOnEntities k sys (io, es) = first (io ++) result
    where
        result = foldr cumulator ([], []) es
        cumulator e (io', es') = bimap (io' ++) (es' ++) (process e)
        process = runSystem k sys


-- TODO if key doesn't exist then don't run system
-- Private
runSystem :: SystemKey -> System -> Entity -> ([IO ()], [Entity])
runSystem key sys e = (io systemOutput, entities)
    where
        entities = e : new systemOutput
        newEntity = insert key (entity systemOutput) e
        systemOutput = sys components
        components = findWithDefault [] key e



{-
    Component:
    Callstack of the Entity. Useful for previous frames, or a single element representing what
    to do for next frame. Remains abstract for the system using a self reference. A system
    itself doesn't represent data, but it's previous transformations can be used to calculate 
    against its own cardinality. A System has no state without a Component. An Entity carries 
    a Component.
-}

type Component = DynamicHolder

-- instance Show Component where
--     show c = show $ metadata c

-- instance Eq Component where
--     (==) c c' = metadata c == metadata c' && value c == value c'


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

