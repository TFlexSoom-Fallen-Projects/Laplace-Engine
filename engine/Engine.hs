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
    replaceComponent,
    singletonEntity,
    addProducer,
    addConsumers,
    addConsumer,

    -- * System
    -- $system
    SystemKey,
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
    consumers :: Map.Map MessageKey [System]
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
data Entity = Entity {
    components :: Map.Map SystemKey Component,
    messages :: Message
}

instance Creatable Entity where
    new = Entity {
        components = Map.empty,
        messages = new :: Message
    }

instance Mergeable Entity where
    merge e e' = e{
        components = mergeUnsafe (components e) (components e'),
        messages = merge (messages (e :: Entity)) (messages (e' :: Entity))
    }

getComponent :: SystemKey -> Entity -> Component
getComponent k e = (!) (components e) k

addComponentWith :: (Component -> Component -> Component)
    -> SystemKey -> Component -> Entity -> Entity
addComponentWith lambda k c e = replaceComponent (Map.insertWith lambda k c (components e)) e

addComponent :: SystemKey -> Component -> Entity -> Entity
addComponent = addComponentWith const

addComponentAssert :: SystemKey -> Component -> Entity -> Entity
addComponentAssert = addComponentWith (error "Key Collision Insert")

replaceComponent :: Map.Map SystemKey Component -> Entity -> Entity
replaceComponent comps e = e{components = comps}

replaceMessages :: Message -> Entity -> Entity
replaceMessages msg e = e{messages=msg}

newEntityFromList :: [Entity -> Entity] -> Entity
newEntityFromList = foldl (\ arg x -> x arg) (new :: Entity)

singletonEntity :: SystemKey -> Component -> Entity
singletonEntity k c = replaceComponent (Map.insert k c Map.empty) (new :: Entity)

addProducer :: MessageKey -> Component -> Entity -> Entity
addProducer k comp e =  replaceMessages newMessage e
    where newMessage = msgAddProducer k comp (messages (e :: Entity))

msgAddProducer :: MessageKey -> Component -> Message -> Message
msgAddProducer k comp msg = msg{producers = Map.insertWith assertion k comp (producers msg)}
    where assertion = error "Producer already exists!"

addConsumers :: MessageKey -> [System] -> Entity -> Entity
addConsumers k systems e = replaceMessages newMessage e
    where newMessage = msgAddConsumers k systems (messages (e :: Entity))

addConsumer :: MessageKey -> System -> Entity -> Entity
addConsumer k sys = addConsumers k [sys]

-- Private
msgAddConsumers :: MessageKey -> [System] -> Message -> Message
msgAddConsumers k systems msg = msg{consumers = Map.insert k newCons consMap}
    where
        consMap = consumers msg
        cons = Map.findWithDefault [] k consMap
        newCons = cons ++ systems

msgAddConsumer :: MessageKey -> System -> Message -> Message
msgAddConsumer k sys = msgAddConsumers k [sys]

{-$system
    =__System:__
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    ==__Laws:__
    1. Every System should have a SystemKey
-}
type System = Component -> SystemOutput

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
        bindedEntities k m = runSystemOnEntities k m es
        matrixOfOutputs = Map.mapWithKey bindedEntities (systems g)
        merged = mergeOutputs es matrixOfOutputs
        resolved = resolveMessages merged
        result = outputsToNewFrame resolved

-- Private
runSystemOnEntities :: SystemKey -> System -> [Entity] -> [Maybe SystemOutput]
runSystemOnEntities k sys = map (runSystem k sys)

-- Private
runSystem :: SystemKey -> System -> Entity -> Maybe SystemOutput
runSystem key sys Entity{components=comps}
    | Map.member key comps = Just (sys component)
    | otherwise = Nothing
    where component = (!) comps key

-- Private
-- TODO use Traversable instead of Map?
mergeOutputs :: [Entity] -> Map.Map SystemKey [Maybe SystemOutput] -> [EntityResolver]
mergeOutputs es = Map.foldr' (zipWith mergeResolver) (map createResolver es)

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
mergeResolver :: Maybe SystemOutput -> EntityResolver -> EntityResolver
mergeResolver (Just SystemOutput{key=k, modified=comp, job=outJobs}) res =
    res {
        modified = addComponentAssert k comp (modified (res :: EntityResolver)),
        job = merge (job (res :: EntityResolver)) outJobs
    }

-- Private
resolveMessages :: [EntityResolver] -> [EntityResolver]
resolveMessages = map (assertEmptyConsumers . resolveMessage)

-- Private
-- The Final Output Should have empty Consumers
resolveMessage :: EntityResolver -> EntityResolver
resolveMessage 
        er@(EntityResolver {
            modified=e, 
            job=j@(EngineJob{
                    messages= Message {
                        producers=p,
                        consumers=c
                    }
            })
        } ) 
    | Map.null c = er
    | Map.null jobsToRes = error "Deadlock. Jobs nonempty with no providers."
    | otherwise = er
        where 
            jobsToRes = Map.intersection c p
            unresolved = Map.difference c p


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
