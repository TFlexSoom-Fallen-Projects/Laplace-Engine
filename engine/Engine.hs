{-# LANGUAGE DuplicateRecordFields #-}
-- |Engine for running the Game containers. Users should be submitting configurations 
-- of Game Objects to the Engine module to be run infinitely. See 'runGame'!
--
-- In contrast, the other provided datatypes ('Entity', 'System', and 'Component') are
-- features for extending the functionality of the engine. It may also be convenient for
-- third parties to implement systems and consolidate system/component groups for entities.
module Engine (
    -- * Component
    -- $component
    Component(..),

    -- * Entity
    -- $entity
    Entity,
    newEntityFromList,
    addEntity,
    getComponent,
    singletonEntity,

    -- * System
    -- $system
    SystemKey,
    SingleInputSystem,
    System,
    SystemOutput(..),

    -- * Game
    -- $game
    Game(..),
    enableSystem,
    runFrame,
    runGame
) where

-- Base Imports
import Data.Map((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(catMaybes)

-- Rewritten Dynamic Wheel for Heterogeneous lists
import Core.Dynamic (Dynamic, DynamicallyAware, DynamicHolder)
import Core.Util (Creatable(..), Mergeable(..), mergeUnsafe, apply, defaultNothing, assert)
import qualified Core.DependencyTree as DependencyTree
import Core.DependencyTree (DependencyTree)

{-$component
    =__Component:__
    Dynamic Data for a entity. Each System has it's own version of a DynamicHolder representing a collection
    of data. Think like fields on a Java Object. These fields of the instance are carried under the tree of data.
    Since the types of the data are hidden from the engine, the engine trusts the systems to be responsible for 
    handling all cases of the data. Poor casts should result in static errors for Engine development and runtime 
    errors for game development
-}

type Component = DynamicHolder

{-$entity
    =__Entity:__
    Data Holder
    Holds the attached/acting Systems on the piece of data represented through Map's key
    Holds possible callstack of previous System Actions (Component)
-}
type Entity = Map.Map SystemKey Component

getComponent :: SystemKey -> Entity -> Component
getComponent k e = (!) e k

addComponentWith :: (Component -> Component -> Component)
    -> SystemKey -> Component -> Entity -> Entity
addComponentWith = Map.insertWith

addComponent :: SystemKey -> Component -> Entity -> Entity
addComponent = addComponentWith const

addComponentAssert :: SystemKey -> Component -> Entity -> Entity
addComponentAssert = addComponentWith (error "Key Collision Insert")

-- Private
getMaybeComponent :: SystemKey -> Entity -> Maybe Component
getMaybeComponent = Map.lookup

newEntityFromList :: [Entity -> Entity] -> Entity
newEntityFromList = foldl (\ arg x -> x arg) Map.empty

singletonEntity :: SystemKey -> Component -> Entity
singletonEntity k c = Map.insert k c Map.empty

{-$system
    =__System:__
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    ==__Laws:__
    1. Every System should have a SystemKey
    2. (len input) == (len output)
-}
type SingleInputSystem = SystemInput -> SystemOutput
type MultiInputSystem = [SystemInput] -> [Maybe SystemOutput]

data System = SINGLE SingleInputSystem
    --                        V Can be any Ord instance
    | BATCH  (SystemInput -> Maybe Int) MultiInputSystem
    | ALL                               MultiInputSystem

{-|
    ==__SystemKey: __
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String
type SharingKey = String
-- TODO Keys would be better as 64 bit integers than strings

type ShareMap = Map.Map SharingKey Component

-- Work for the engine that is not dependent on the entity
data EngineJob = EngineJob {
    io :: [IO ()],
    added :: [Entity]
}

instance Creatable EngineJob where
    new = EngineJob {
        io = [],
        added = []
    }

instance Mergeable EngineJob where
    merge job job' = job {
        io = io job ++ io job',
        added = added job ++ added job'
    }

-- Work for the engine that is dependent on the entity
data Modification = Modification {
    modified :: Component,
    delete :: Bool,
    shared :: ShareMap
}

-- We cannot merge or create modification due to component.

-- Tuple Job and Modification for Output of each system
data SystemOutput = SystemOutput {
    modification :: Modification,
    job :: EngineJob
}

data SystemInput = SystemInput {
    shared :: ShareMap,
    component :: Component
}

toSystemInput :: ShareMap -> Component -> SystemInput
toSystemInput shared comp = SystemInput {
    shared = shared,
    component = comp
}

{-$game
    =__Game:__
    List of entities which can be filtered with system system key map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
data Game = Game {
    systems :: Map.Map SystemKey System,
    dependency :: DependencyTree.DependencyTree SystemKey,
    entities :: [Entity]
}

instance Creatable Game where
    new = Game {
        systems = Map.empty,
        dependency = DependencyTree.empty,
        entities = []
    }

instance Mergeable Game where
    merge g g' = Game {
        systems = mergeUnsafe (systems g) (systems g'),
        -- TODO Shouldn't this be merge?
        dependency = DependencyTree.union (dependency g) (dependency g'),
        entities = entities g ++ entities g'
    }

enableSystem :: SystemKey -> System -> Game -> Game
enableSystem key sys g@(Game{systems=sysMap, dependency=deps}) =
    (.) (replaceSystems newSystems) (replaceDependency newDependency) g
    where
        newSystems = Map.insert key sys sysMap
        newDependency = DependencyTree.insert key deps

-- Copied from enabled system due to DependencyTree optimization
enableSystemAfter :: SystemKey -> System -> [SystemKey] -> Game -> Game
enableSystemAfter key sys keys g@(Game{systems=sysMap, dependency=deps}) =
    (.) (replaceSystems newSystems) (replaceDependency newDependency) g
    where
        newSystems = Map.insert key sys sysMap
        newDependency = foldr (`DependencyTree.depend` key) deps keys

replaceSystems :: Map.Map SystemKey System -> Game -> Game
replaceSystems sys g = g{systems = sys}

replaceDependency :: DependencyTree SystemKey -> Game -> Game
replaceDependency dep g = g{dependency=dep}

replaceEntities :: [Entity] -> Game -> Game
replaceEntities e g = g{entities = e}

addEntity :: Entity -> Game -> Game
addEntity entity g = replaceEntities (entities g ++ [entity]) g

runGame :: Game -> IO ()
runGame game = do {
    (io, modifiedGame) <- return (runFrame game);
    foldr (>>) (pure ()) io;
    runGame modifiedGame
}

runFrame :: Game -> ([IO ()], Game)
runFrame g@(Game{
        systems=sysMap,
        dependency=dep,
        entities=es
    }) = (fst result, replaceEntities (snd result) g)
    where
        sysFolds = foldForOutputs dep (sysMap !) (newSystemOutputFolds es)
        result = outputsToNewFrame sysFolds


-- Private
data SystemOutputFold = SystemOutputFold {
    jobs :: EngineJob,
    modified :: Entity,
    shared :: ShareMap,
    delete :: Bool
}

newSystemOutputFold :: Entity -> SystemOutputFold
newSystemOutputFold e = SystemOutputFold {
    jobs = new :: EngineJob,
    modified = e,
    shared = Map.empty,
    delete = False
}

newSystemOutputFolds :: [Entity] -> [SystemOutputFold]
newSystemOutputFolds = map newSystemOutputFold

-- It is assumed that the systme output has to do with the same entity as the 
-- SystemOutputFold. Thus the modified entity stays the same through the frame.
applyOutputToFold :: SystemKey -> Maybe SystemOutput -> SystemOutputFold -> SystemOutputFold
applyOutputToFold _ Nothing fold = fold
applyOutputToFold
    key

    (Just SystemOutput {
        modification = Modification {modified=comp, delete=del', shared=shared'},
        job = job'
    })

    fold@(SystemOutputFold{jobs=job, modified=mod, shared=shared, delete=del})

    = fold {
        jobs = merge job job',
        modified = Map.insert key comp mod,
        shared = Map.union shared shared',
        delete = del || del'
    }


applyOutputToFolds :: SystemKey -> [Maybe SystemOutput] -> [SystemOutputFold] -> [SystemOutputFold]
applyOutputToFolds key = zipWith (applyOutputToFold key)

-- I considered having a readonly list of entities here... 
-- but we are doing some major assertion work anyways and the computation
-- seemed to get pretty unoptimized when working with a secondary list outside of SystemOutputFold
-- Consider: Maybe include readonly entity in SystmeOutputFold datatype.
foldForOutputs :: DependencyTree SystemKey -> (SystemKey -> System) -> [SystemOutputFold] -> [SystemOutputFold]
foldForOutputs deps sysGetter sysFolds = DependencyTree.foldr' folder sysFolds deps
    where
        folder key sysFolds' = applyOutputToFolds key (getOutput key (map (getMaybeInput key) sysFolds')) sysFolds'
        getOutput key' = runSystem (sysGetter key')
        getMaybeInput key SystemOutputFold{modified=e, shared=shared}
            | Map.member key e =
                Just SystemInput { shared = shared, component = (!) e key }
            | otherwise = Nothing


-- Private
runSystem :: System -> [Maybe SystemInput] -> [Maybe SystemOutput]
runSystem (SINGLE sys) = map (runSingleSystem sys)
runSystem (BATCH filter sys) = runMultiSystem filter sys
runSystem (ALL sys) = runMultiSystem (const (Just 0)) sys

-- Private
runSingleSystem :: SingleInputSystem -> Maybe SystemInput -> Maybe SystemOutput
runSingleSystem sys = defaultNothing (Just . sys)

-- TODO
-- Private
runMultiSystem :: (SystemInput -> Maybe Int) -> MultiInputSystem -> [Maybe SystemInput] -> [Maybe SystemOutput]
runMultiSystem filter sys maybeInputs = keepOrder (concatMap sys inputs) maybeInputs
    where
        inputs = [catMaybes maybeInputs] 

-- Private
keepOrder :: [Maybe SystemOutput] -> [Maybe SystemInput] -> [Maybe SystemOutput]
keepOrder [] [] = []
keepOrder [] ((Just y) : ys) = error "System Gave Smaller Number of Outputs"
keepOrder (x : xs) [] = error "System Gave Larger Number of Outputs"
keepOrder (x : xs) ((Just y) : ys) = x : keepOrder xs ys
keepOrder lst (Nothing : ys) = Nothing : keepOrder lst ys

-- Private
outputsToNewFrame :: [SystemOutputFold] -> ([IO ()], [Entity])
outputsToNewFrame = foldr outputToNewFrame ([], [])

-- Private
outputToNewFrame :: SystemOutputFold -> ([IO ()], [Entity]) -> ([IO ()], [Entity])
outputToNewFrame SystemOutputFold{modified=mod, jobs=EngineJob{io=ioj, added=addedj}, delete=False} (ios, es) = (ios ++ ioj, mod : addedj)
outputToNewFrame SystemOutputFold{modified=mod, jobs=EngineJob{io=ioj, added=addedj}, delete=True} (ios, es) = (ios ++ ioj, addedj)
