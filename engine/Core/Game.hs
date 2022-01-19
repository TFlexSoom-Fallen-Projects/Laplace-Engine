{-# LANGUAGE DuplicateRecordFields #-}
{-|
    =__Game:__
    List of entities which can be filtered with system system key map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
module Core.Game (
    Game(..),
    enableSystem,
    enableSystemsAfter,
    addEntity,
    runFrames,
    runGame
) where

-- Base Imports
import Data.Map((!))
import qualified Data.Map as Map
import Data.Maybe(fromMaybe, catMaybes, mapMaybe)

import Core.SystemKey (SystemKey)
import Core.Component (Component(..))
import Core.Entity (Entity)
import Core.System (
    SingleInputSystem,
    MultiInputSystem,
    System(..),
    
    SystemKey,
    SharingKey,
    ShareMap,
    EngineJob(..),
    Modification(..),
    SystemInput(..),
    toSystemInput,
    SystemOutput(..),
    )

import Core.Util (Creatable(..), Mergeable(..), mergeUnsafe, apply, defaultNothing, assert)
import qualified Core.DependencyTree as DependencyTree
import Core.DependencyTree (DependencyTree)

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
enableSystemsAfter :: SystemKey -> System -> [SystemKey] -> Game -> Game
enableSystemsAfter key sys keys g@(Game{systems=sysMap, dependency=deps}) =
    (.) (replaceSystems newSystems) (replaceDependency newDependency) g
    where
        newSystems = Map.insert key sys sysMap
        newDependency = foldr (`DependencyTree.depend` key) deps keys

-- Private
replaceSystems :: Map.Map SystemKey System -> Game -> Game
replaceSystems sys g = g{systems = sys}

-- Private
replaceDependency :: DependencyTree SystemKey -> Game -> Game
replaceDependency dep g = g{dependency=dep}

-- Private
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

runFrames :: Int -> Game -> IO ()
runFrames 0 _ = pure ()
runFrames num game = do {
    (io, modifiedGame) <- return (runFrame game);
    foldr (>>) (pure ()) io;
    runFrames (num - 1) modifiedGame
}

-- Private
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

-- Private
newSystemOutputFold :: Entity -> SystemOutputFold
newSystemOutputFold e = SystemOutputFold {
    jobs = new :: EngineJob,
    modified = e,
    shared = Map.empty,
    delete = False
}

-- Private
newSystemOutputFolds :: [Entity] -> [SystemOutputFold]
newSystemOutputFolds = map newSystemOutputFold

-- Private
-- It is assumed that the system output has to do with the same entity as the 
-- SystemOutputFold. Thus the modified entity stays the same through the frame.
applyOutputToFold :: SystemKey -> Maybe SystemOutput -> SystemOutputFold -> SystemOutputFold
applyOutputToFold _ Nothing fold = fold
applyOutputToFold
    key

    (Just SystemOutput {
        modification = Modification {modified=comp, delete=del', newShares=shared'},
        job = job'
    })

    fold@(SystemOutputFold{jobs=job, modified=mod, shared=shared, delete=del})

    = fold {
        jobs = merge job job',
        modified = Map.insert key comp mod,
        shared = Map.union shared shared',
        delete = del || del'
    }

-- Private
applyOutputToFolds :: SystemKey -> [Maybe SystemOutput] -> [SystemOutputFold] -> [SystemOutputFold]
applyOutputToFolds key = zipWith (applyOutputToFold key)

-- Private
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

-- Private
runMultiSystem :: (SystemInput -> Maybe Int) -> MultiInputSystem -> [Maybe SystemInput] -> [Maybe SystemOutput]
runMultiSystem filter sys maybeInputs = keepOrder outputs maybeInputs
    where
        inputs = catMaybes maybeInputs
        maybeBatches = map filter inputs
        batches = catMaybes maybeBatches
        mapBatchToInputs = foldr folder Map.empty (zip batches inputs)
        folder (num, input) = Map.alter (Just . (:) input . fromMaybe []) num
        mapBatchToOutputs = Map.map sys mapBatchToInputs
        outputs = batchToInOrder mapBatchToOutputs (reverse maybeBatches)

-- Private
batchToInOrder :: Map.Map Int [Maybe SystemOutput] -> [Maybe Int] -> [Maybe SystemOutput]
batchToInOrder m [] | Map.null m = []
                    | otherwise = error "System Provided More Outputs than Inputs"
batchToInOrder m (Nothing : xs) = Nothing : batchToInOrder m xs
batchToInOrder m ((Just x) : xs)
                    | Map.member x m && not (null lst) = head lst : batchToInOrder (Map.insert x (tail lst) m) xs
                    | otherwise = error "Engine Mapping Error"
    where lst = (!) m x

-- Private
-- Assumes first list is in the same order as the second
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
outputToNewFrame SystemOutputFold{modified=mod, jobs=EngineJob{io=ioj, added=addedj}, delete=False} (ios, es) = (ios ++ ioj, mod : addedj ++ es)
outputToNewFrame SystemOutputFold{modified=mod, jobs=EngineJob{io=ioj, added=addedj}, delete=True} (ios, es) = (ios ++ ioj, addedj ++ es)
