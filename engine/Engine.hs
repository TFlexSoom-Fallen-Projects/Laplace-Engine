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

-- Rewritten Dynamic Wheel for Heterogeneous lists
import Core.Dynamic (Dynamic, DynamicallyAware, DynamicHolder)
import Core.Util (Creatable(..), Mergeable(..), mergeUnsafe, apply, defaultNothing)
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
-}
type SingleInputSystem = SystemInput -> SystemOutput

data System = SINGLE SingleInputSystem
    --                     V Can be any Ord instance
    | BATCH  (Component -> Int) ([SystemInput] -> [Maybe SystemOutput])
    | ALL                       ([SystemInput] -> [Maybe SystemOutput])

{-|
    ==__SystemKey: __
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String
type SharingKey = String
-- TODO Keys would be better as 64 bit integers than strings

type ShareMap = Map.Map SharingKey Component

data EngineJob = EngineJob {
    io :: [IO ()],
    added :: [Entity],
    delete :: Bool
}

instance Creatable EngineJob where
    new = EngineJob {
        io = [],
        added = [],
        delete = False
    }

instance Mergeable EngineJob where
    merge job job' = job {
        io = io job ++ io job',
        added = added job ++ added job',
        delete = delete job || delete job'
    }

data Modification = Modification {
    modified :: Component,
    job :: EngineJob
}

data SystemOutput = SystemOutput {
    modification :: Modification,
    shared :: ShareMap
}

newSystemOutput :: SystemKey -> Component -> SystemOutput
newSystemOutput k comp = SystemOutput {
    key = k,
    shared = Map.empty,
    modified = comp,
    job = new :: EngineJob
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
        systems=Map.empty,
        dependency= DependencyTree.empty,
        entities=[]
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
        outputs = foldForOutputs dep (sysMap !) es

        bindedEntities k m = runSystem k m es
        matrixOfOutputs = Map.mapWithKey bindedEntities (systems g)
        merged = mergeOutputs es matrixOfOutputs
        -- resolved = resolveEntityMessages merged
        result = outputsToNewFrame merged


-- Private
data SystemFold = SystemFold {
    modifications :: [Maybe Modification],
    shared :: ShareMap
}

newSystemFold :: Int -> SystemFold
newSystemFold len = SystemFold {
    modifications = take len (repeat Nothing),
    shared = Map.empty
}

instance Mergeable SystemFold where
    merge 
        fold@(SystemFold{modifications=mods, shared=shared'}) 
        SystemFold{modifications=mods', shared=shared'} = 
            fold{
                modifications 
            }

foldForOutputs :: DependencyTree SystemKey -> (SystemKey -> System) -> [Entity] -> [Maybe Modification]
foldForOutputs deps sysGetter es = DependencyTree.foldr' transform (newSystemFold es) deps
    where transform key fold = 

-- bindededSystems k mappedOutputs =  Map.insert k (runSystem ((!) sysMap) ) mappedOutputs
-- outputs = DependencyTree.foldr'


-- Private
runSystem :: SystemKey -> System -> [SystemKey -> Maybe SystemInput] -> [Maybe SystemOutput]
runSystem k (SINGLE sys) = map (runSingleSystem k sys)
-- runSystem k (BATCH filter sys) = 
-- runSystem k (ALL sys) = sys 

-- Private
runSingleSystem :: SystemKey -> SingleInputSystem -> (SystemKey -> Maybe SystemInput) -> Maybe SystemOutput
runSingleSystem key sys = (.) (defaultNothing (Just . sys)) (apply key)

--Private
data SystemOutputResolver = SystemOutputResolver {
    modified :: Entity,
    job :: EngineJob
}


-- Private
-- TODO use Traversable instead of Map?
mergeOutputs :: [Entity] -> Map.Map SystemKey [Maybe SystemOutput] -> [SystemOutputResolver]
mergeOutputs es = Map.foldr' (zipWith mergeMaybeResolver) (map createResolver es)

-- Private
createResolver :: Entity -> SystemOutputResolver
createResolver e = SystemOutputResolver {
    modified = e,
    job = new :: EngineJob
}

-- Private
mergeMaybeResolver :: Maybe SystemOutput -> SystemOutputResolver -> SystemOutputResolver
mergeMaybeResolver (Just out@(SystemOutput{key=k, modified=comp, job=outJobs})) = mergeResolver out
mergeMaybeResolver _ = id

-- Private
mergeResolver :: SystemOutput -> SystemOutputResolver -> SystemOutputResolver
mergeResolver SystemOutput{key=k, modified=comp, job=outJobs} res =
    res {
        modified = addComponentAssert k comp (modified (res :: SystemOutputResolver)),
        job = merge (job (res :: SystemOutputResolver)) outJobs
    }

-- Private
outputsToNewFrame :: [SystemOutputResolver] -> ([IO ()], [Entity])
outputsToNewFrame = foldr outputToNewFrame ([], [])

-- Private
outputToNewFrame :: SystemOutputResolver -> ([IO ()], [Entity]) -> ([IO ()], [Entity])
outputToNewFrame SystemOutputResolver{modified=mod, job=EngineJob{io=ioj, added=addedj, delete=False}} (ios, es) = (ios ++ ioj, mod : addedj)
outputToNewFrame SystemOutputResolver{modified=mod, job=EngineJob{io=ioj, added=addedj, delete=True}} (ios, es) = (ios ++ ioj, addedj)
