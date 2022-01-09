{-# LANGUAGE DuplicateRecordFields #-}
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

    -- * Message
    -- $message
    MessageKey,
    Message,

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

-- Rewritten Dynamic Wheel for Heterogeneous lists
import Dynamic (Dynamic, DynamicallyAware, DynamicHolder)

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

instance Mergeable a => Mergeable ( Maybe a ) where
    merge (Just a) (Just b) = Just (merge a b)
    merge (Just a) _ = Just a
    merge _ (Just b) = Just b
    merge _ _ = Nothing

-- | Use to assert that when 2 maps Merge they do not collide
mergeUnsafe :: Ord k => Map.Map k a -> Map.Map k a -> Map.Map k a
mergeUnsafe = Map.unionWith (error "Key Collision Union")

{-$component
    =__Component:__
    Dynamic Data for a entity. Each System has it's own version of a DynamicHolder representing a collection
    of data. Think like fields on a Java Object. These fields of the instance are carried under the tree of data.
    Since the types of the data are hidden from the engine, the engine trusts the systems to be responsible for 
    handling all cases of the data. Poor casts should result in static errors for Engine development and runtime 
    errors for game development
-}

type Component = DynamicHolder

-- TODO For MessageKey and SystemKey --> 64 bit ints would be more optimal than Strings
type MessageKey = String

{-$message
    =__Message:__
    Messages are a way for systems to communicate data to eachother within the frame/iteration. I many cases,
    systems can couple/communicate with other systems by creating entities with respective components. For all
    work on entities that are needed outside of the scope of the system, Messages provide a way to interface
    without coupling with these other systems: producing and interface with 'producers' or consuming
    an interface with 'consumers'

-}
data Message = Message {
    producers :: Map.Map MessageKey Component,
    consumers :: Map.Map MessageKey [SingleInputSystem]
}

instance Creatable Message where
    new = Message {
        producers = Map.empty,
        consumers = Map.empty
    }

instance Mergeable Message where
    merge m m' = m {
        producers = mergeUnsafe (producers m) (producers m'),
        consumers = Map.unionWith (++) (consumers m) (consumers m')
    }

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
newEntityFromList = foldl (\ arg x -> x arg) (new :: Entity)

singletonEntity :: SystemKey -> Component -> Entity
singletonEntity k c = Map.insert k c (new :: Entity)

{-$system
    =__System:__
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    ==__Laws:__
    1. Every System should have a SystemKey
-}
type SingleInputSystem = Component -> SystemOutput

data System = SINGLE SingleInputSystem
    --                     V Can be any Ord instance
    | BATCH  (Component -> Int) ([Component] -> [Maybe SystemOutput])
    | ALL                       ([Component] -> [Maybe SystemOutput])

{-|
    ==__SystemKey: __
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String

data EngineJob = EngineJob {
    io :: [IO ()],
    messages :: Message,
    added :: [Entity],
    delete :: Bool
}

instance Creatable EngineJob where
    new = EngineJob {
        io = [],
        messages = new :: Message,
        added = [],
        delete = False
    }

instance Mergeable EngineJob where
    merge job job' = job {
        io = io job ++ io job',
        messages = merge (messages (job :: EngineJob)) (messages (job' :: EngineJob)),
        added = added job ++ added job',
        delete = delete job || delete job'
    }


data SystemOutput = SystemOutput {
    key :: SystemKey,
    modified :: Component,
    job :: EngineJob
}

newSystemOutput :: SystemKey -> Component -> SystemOutput
newSystemOutput k comp = SystemOutput {
    key = k,
    modified = comp,
    job = new :: EngineJob
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

enableSystem :: String -> System -> Game -> Game
enableSystem key sys g = replaceSystems newSystems g
    where newSystems = Map.insert key sys (systems g)

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
runFrame g = (fst result, replaceEntities (snd result) g)
    where
        es = entities g
        bindedEntities k m = runSystem k m es
        matrixOfOutputs = Map.mapWithKey bindedEntities (systems g)
        merged = mergeOutputs es matrixOfOutputs
        resolved = resolveEntityMessages merged
        result = outputsToNewFrame resolved

-- Private
runSystem :: SystemKey -> System -> [Entity] -> [Maybe SystemOutput]
runSystem k (SINGLE sys) = map (runSingleSystem k sys)
-- runSystem k (BATCH filter sys) = 
-- runSystem k (ALL sys) = sys 

-- Private
runSingleSystem :: SystemKey -> SingleInputSystem -> Entity -> Maybe SystemOutput
runSingleSystem key sys e = sys =<< getMaybeComponent key e

-- Private
-- TODO use Traversable instead of Map?
mergeOutputs :: [Entity] -> Map.Map SystemKey [Maybe SystemOutput] -> [EntityResolver]
mergeOutputs es = Map.foldr' (zipWith mergeMaybeResolver) (map createResolver es)

--Private
data EntityResolver = EntityResolver {
    modified :: Entity,
    job :: EngineJob
}

-- Private
createResolver :: Entity -> EntityResolver
createResolver e = EntityResolver {
    modified = e,
    job = new :: EngineJob
}

-- Private
mergeMaybeResolver :: Maybe SystemOutput -> EntityResolver -> EntityResolver
mergeMaybeResolver (Just out@(SystemOutput{key=k, modified=comp, job=outJobs})) = mergeResolver out
mergeMaybeResolver _ = id

-- Private
mergeResolver :: SystemOutput -> EntityResolver -> EntityResolver
mergeResolver SystemOutput{key=k, modified=comp, job=outJobs} res =
    res {
        modified = addComponentAssert k comp (modified (res :: EntityResolver)),
        job = merge (job (res :: EntityResolver)) outJobs
    }

-- Private
resolveEntityMessages :: [EntityResolver] -> [EntityResolver]
resolveEntityMessages = map (assertEmptyConsumers . resolveEntityMessage)

-- Private
-- The Final Output Should have empty Consumers
resolveEntityMessage :: EntityResolver -> EntityResolver
resolveEntityMessage
        er@(EntityResolver {
            modified=e,
            job=j@(EngineJob{
                    messages= msg@(Message {
                        producers=p,
                        consumers=c
                    })
            })
        } )
    | Map.null c = er
    | Map.null jobsToRes = error "Deadlock. Jobs nonempty with no providers."
    | otherwise = resolveEntityMessage (foldr mergeResolver newResolver (resolveMessage p jobsToRes))
        where
            jobsToRes = Map.intersection c p
            unresolved = Map.difference c p
            newResolver = er{job=j{messages=msg{consumers=unresolved}}} :: EntityResolver

-- Private
resolveMessage :: Map.Map MessageKey Component -> Map.Map MessageKey [System] -> [SystemOutput]
resolveMessage producers = Map.foldrWithKey (consumeAndConcat producers) []
    where consumeAndConcat p k sysArr = (++) (map (\sys -> sys ((!) p k)) sysArr)

-- Private
assertEmptyConsumers :: EntityResolver -> EntityResolver
assertEmptyConsumers e@(EntityResolver{ job=EngineJob {messages= Message { consumers=c } } })
    | Map.null c = e
    | otherwise = error "Consumers are Non-Empty!"

-- Private
outputsToNewFrame :: [EntityResolver] -> ([IO ()], [Entity])
outputsToNewFrame = foldr outputToNewFrame ([], [])

-- Private
outputToNewFrame :: EntityResolver -> ([IO ()], [Entity]) -> ([IO ()], [Entity])
outputToNewFrame EntityResolver{modified=mod, job=EngineJob{io=ioj, added=addedj, delete=False}} (ios, es) = (ios ++ ioj, mod : addedj)
outputToNewFrame EntityResolver{modified=mod, job=EngineJob{io=ioj, added=addedj, delete=True}} (ios, es) = (ios ++ ioj, addedj)
