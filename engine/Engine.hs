{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- |Engine for running the Game containers. Users should be submitting configurations 
-- of Game Objects to the Engine module to be run infinitely. See 'runGame'!
--
-- In contrast, the other provided datatypes ('Entity', 'System', and 'Component') are
-- features for extending the functionality of the engine. It may also be convenient for
-- third parties to implement systems and consolidate system/component groups for entities.
module Engine (
    -- * Engine Utilities
    -- $utility
    Creatable,
    Mergeable,

    -- * Component
    -- $component
    Component(..),

    -- * Entity
    -- $entity
    Entity,
    newEntityFromList,
    addEntity,
    getComponent,
    replaceComponent,
    singletonEntity,
    MessageKey,
    addProducer,
    addConsumers,
    addConsumer,
    markForDeletion,

    -- * System
    -- $system
    SystemKey,
    enableSystem,
    System,
    SystemOutput(..),

    -- * Game
    -- $game
    Game(..),
    runFrame,
    runGame
) where

-- Base Imports
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(mapMaybe)

-- Rewritten Dynamic Wheel for Heterogeneous lists
import Dynamic (Dynamic, DynamicallyAware, DynamicHolder)

-- TODO should have I have Runtime Defined Modules?

{-$utility
    =__Utility:__
    Utilities for help in defining structures and functionality for the engine.
    The goal of these is to avoid repeating myself. As I coded the engine
    I found myself chasing typical patterns. I decided to classify these.

    This may include some additional classes and assertion utilities in the future
-}

-- | Anything that can be created and "zero" valued
class Creatable a where
    -- | Creates a new instance of the datatype
    new :: a

-- | Anything that can be safely merged with itself
class Mergeable a where
    -- | Merges first two instances and outputs the combinations
    merge :: a -> a -> a

-- | Use to assert that when 2 maps Merge they do not collide
mergeUnsafe :: Map.Map k a -> Map.Map k a -> Map.Map k a
mergeUnsafe = Map.unionWith (error "Key Collision")

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
data Entity = Entity{
    frameID :: FrameID,
    components :: Map.Map SystemKey Component,
    producers :: Map.Map MessageKey Component,
    consumers :: Map.Map MessageKey [System],
    delete :: Bool
}

{-|
    ==__FrameID:__
    Key to verify entities merge together correctly inbetween frames/operations
-}
type FrameID = Int

-- TODO For MessageKey and SystemKey --> 64 bit ints would be more optimal than Strings
type MessageKey = String


instance Creatable Entity where
    new = Entity{
        frameID = 0,
        components = Map.empty,
        producers = Map.empty,
        consumers = Map.empty,
        delete = False
    }

instance Mergeable Entity where
    merge e e' = e{
        components = mergeUnsafe (components e) (components e'),
        producers = mergeUnsafe (producers e) (producers e'),
        consumers = mergeUnsafe (consumers e) (consumers e'),
        delete = delete e || delete e'
    }

instance Ord Entity where
    (<=) e e' = (<=) (frameID e) (frameID e')

getComponent :: SystemKey -> Entity -> Component
getComponent k e = (!) k (components e)

replaceComponent :: SystemKey -> Component -> Entity -> Entity
replaceComponent k c e = e {components = Map.insert k c (components e)}

newEntityFromList :: [Entity -> Entity] -> Entity
newEntityFromList = foldl (\ arg x -> x arg) newEntity

singletonEntity :: SystemKey -> Component -> Entity
singletonEntity k c = replaceComponent k c new

-- private
replaceFrameID :: FrameID -> Entity -> Entity
replaceFrameID id e = e{frameID = id}

addProducer :: MessageKey -> Component -> Entity -> Entity
addProducer k comp e = e{producers = Map.insertWith assertion k comp (producers e)}
    where assertion = Error "Producer already exists!"

addConsumers :: MessageKey -> [System] -> Entity -> Entity
addConsumers k systems e = e{consumers = Map.insert k newCons consMap}
    where 
        consMap = consumers e
        cons = Map.findWithDefault [] k consMap
        newCons = cons ++ systems

addConsumer :: MessageKey -> System -> Entity -> Entity
addConsumer k sys = addConsumers k [sys]

markForDeletion :: Entity -> Entity
markForDeletion e = e{delete = True}

{-$system
    =__System:__
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    ==__Laws:__
    1. Every System should have a SystemKey
-}
type System = Entity -> SystemOutput

{-|
    ==__SystemKey: __
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String

enableSystem :: String -> System -> Game -> Game
enableSystem key sys = replaceSystems newSystems
    where newSystems = Map.insert key sys (systems g)

data SystemOutput = SystemOutput {
    io :: [IO ()],
    modified :: Entity,
    added :: [Entity]
}

instance Creatable SystemOutput where
    new = SystemOutput {
        io = [],
        modified = newEntity,
        added = []
    }

instance Mergeable SystemOutput where
    merge out out' = SystemOutput {
        io = io out ++ io out',
        modified = modified out `merge` modified out',
        added = new out ++ new out'
    }



{-$game
    =__Game:__
    List of entities which can be filtered with system system key map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
data Game = Game {
    systems :: Map.Map SystemKey System,
    entities :: [Entity]
}

instance Creatable Game where
    new = Game {
        systems=Map.empty,
        entities=[]
    }

instance Mergeable Game where
    merge g g' = Game {
        systems = mergeUnsafe (systems g) (systems g'),
        entities = entities g ++ entities g'
    }

replaceSystems :: Map.Map SystemKey System -> Game -> Game
replaceSystems sys g = g{systems = sys}

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
runFrame g = (io' merged, replaceEntities replacements g)
    where
        es = frameIDEntities (entities g)
        bindedEntities k m = runSystemOnEntities k m es
        outputs = mapWithKey bindedEntities (systems g)
        merged = mergeOutputs outputs
        ls = Set.toAscList (modified' merged)
        resolved = resolveMessages ls
        replacements = resolved ++ added' merged

-- Private
frameIDEntities :: [Entity] -> [Entity]
frameIDEntities (e:es) = e{frameID=1 + length es} : frameIDEntities es

-- Private
mergeOutputs :: [SystemOutputSet] -> SystemOutputSet
mergeOutputs = foldr localMerge new
    where
        outMod = modified' out
        resMod = modified' res
        mergedComps = mergeComponents
            (Set.toAscList (Set.intersection outMod resMod))
            (Set.toAscList (Set.intersection resMod outMod))

        localMerge out res =
            res{
                io' = io' res ++ io' out,
                modified' = Set.union resMod (Set.fromList mergedComps),
                added' = added' res ++ added' out
            }

-- Private
mergeComponents :: [Entity] -> [Entity] -> [Entity]
mergeComponents = zipWith merge

resolveMessages :: [Entity] -> [Entity]
resolveMessages = map resolveMessage

resolveMessage :: Entity -> Entity
resolveMessage e = e

-- Private
data SystemOutputSet = SystemOutputSet {
    io' :: [IO ()],
    modified' :: Set.Set Entity,
    added' :: [Entity]
}

-- Private
instance Creatable SystemOutputSet where
    new = SystemOutputSet { io' = [], modified' = Set.empty, added' = []}

-- Private
runSystemOnEntities :: SystemKey -> System -> [Entity] -> SystemOutputSet
runSystemOnEntities k sys es = foldr concatSystemOutput new outputs
    -- foldr is important because of concatSystemOutput's modified creation
    -- We want to make sure entities are not reversed each iteration
    where
        outputs = mapMaybe (runSystem k sys) es
        clean = cleanSystemRun outputs

-- Private
concatSystemOutput :: SystemOutput -> SystemOutputSet -> SystemOutputSet
concatSystemOutput
    SystemOutput{io=io, modified=mod, added=add}
    SystemOutputSet{io'=ioSet, modified'=modSet, added'=addSet}
    = SystemOutputSet{
        io' = ioSet ++ io,
        modified' = Set.insert mod modSet ,
        added' = addSet ++ add
    }

-- Private
runSystem :: SystemKey -> System -> Entity -> Maybe SystemOutput
runSystem key sys Entity{components=comps, frameID=id}
    | Map.member key comps = Just (sys entityLens)
    | otherwise = Nothing
    where
        entityLens =  singletonEntity key ((Map.!) comps key) `withFrameID` id
        withFrameID = flip replaceFrameID
