{-# LANGUAGE DuplicateRecordFields #-}
{-|
    =__Engine:__
    Implementation of all classes
-}
module Core.Engine (
    GameImpl,
    game
) where

-- Base Imports
import Data.Map((!))
import qualified Data.Map as Map
import Data.Maybe(fromJust, fromMaybe, catMaybes, mapMaybe)

import Core.SystemKey ( SystemKey )
import Core.Component ( Component(..) )
import Core.Entity ( Entity )
import Core.System (
    Priority,
    SharingKey,
    Perspective(..),
    SingleInputSystem,
    MultiInputSystem,
    SystemImpl(..),
    System(..)
    )
import Core.Game( Game(..) )

import Core.Util (Creatable(..), Mergeable(..), mergeUnsafe, apply, defaultNothing, assert)
import qualified Core.DependencyTree as DependencyTree
import Core.DependencyTree (DependencyTree)


game :: GameImpl
game = new :: GameImpl


-------------------------   IMPLEMENTATION BELOW   ----------------------------

type Context = Entity

data GameImpl = GameImpl {
    systems :: Map.Map SystemKey (SystemImpl ScopedModification),
    dependency :: DependencyTree.DependencyTree SystemKey,
    context :: Context,
    entities :: [Entity]
}

replaceSystems :: Map.Map SystemKey (SystemImpl ScopedModification) -> GameImpl -> GameImpl
replaceSystems sys g = g{systems = sys}

replaceDependency :: DependencyTree SystemKey -> GameImpl -> GameImpl
replaceDependency dep g = g{dependency=dep}

replaceEntities :: [Entity] -> GameImpl -> GameImpl
replaceEntities e g = g{entities = e}

replaceContext :: Entity -> GameImpl -> GameImpl
replaceContext e g = g{context = e}

-- Private
runFrame :: GameImpl -> ([IO ()], GameImpl)
runFrame g@GameImpl{
        systems=sysMap,
        dependency=dep,
        entities=es
    } = (fst result, replaceEntities (snd result) g)
    where
        sysFolds = foldOverModifications dep (sysMap !) ((context :: GameImpl -> Context) g) (newModifications es)
        result = outputsToNewFrame sysFolds

instance Creatable GameImpl where
    new = GameImpl {
        systems = Map.empty,
        dependency = DependencyTree.empty,
        context = Map.empty,
        entities = []
    }

instance Mergeable GameImpl where
    merge g g' = g {
        systems = mergeUnsafe (systems g) (systems g'),
        -- TODO Shouldn't this be merge?
        dependency = DependencyTree.union (dependency g) (dependency g'),
        context = mergeUnsafe ((context :: GameImpl -> Context) g) ((context :: GameImpl -> Context) g'),
        entities = entities g ++ entities g'
    }

instance Game GameImpl where
    enable sys g@GameImpl{systems=sysMap, dependency=deps, context=ctxt} =
        (.) (replaceSystems newSystems) (
            (.) (replaceDependency newDependency)
                (replaceContext newContext)
        )
            g
        where
            key = getKey sys
            sysImpl = getImplementation sys
            sysCtxt = initContext sys
            newSystems = Map.insert key sysImpl sysMap
            newDependency = DependencyTree.insert key deps
            newContext = Map.insert key sysCtxt ctxt

    addEntity entity g = replaceEntities (entities g ++ [entity]) g

    runFrames 0 _ = pure ()
    runFrames num game = do {
        (io, modifiedGame) <- return (runFrame game);
        foldr (>>) (pure ()) io;
        runFrames (num - 1) modifiedGame
    }

    runGame game = do {
        (io, modifiedGame) <- return (runFrame game);
        foldr (>>) (pure ()) io;
        runGame modifiedGame
    }


-- TODO Subject to change
-- Copied from enabled system due to DependencyTree optimization
-- enableSystemsAfter :: SystemKey -> System -> [SystemKey] -> Game -> Game
-- enableSystemsAfter key sys keys g@GameImpl{systems=sysMap, dependency=deps} =
--     (.) (replaceSystems newSystems) (replaceDependency newDependency) g
--     where
--         newSystems = Map.insert key sys sysMap
--         newDependency = foldr (`DependencyTree.depend` key) deps keys



type ShareMap = Map.Map SharingKey Component

-- Private
data Modification = Modification {
    io :: [IO ()],
    added :: [Entity],
    owner :: Entity,
    shared :: ShareMap,
    delete :: Bool
}

newModification :: Entity -> Modification
newModification e = Modification {
    io = [],
    added = [],
    owner = e,
    shared = Map.empty,
    delete = False
}

newModifications :: [Entity] -> [Modification]
newModifications = map newModification

instance Mergeable Modification where
    merge 
        mod@Modification{io=io, added=added, owner=owner, shared=shared, delete=del} 
        Modification{io=io', added=added', owner=owner', shared=shared', delete=del'} 
        =
        mod{
            io = io ++ io',
            added = added ++ added',
            owner = Map.union owner owner',
            shared = Map.union shared shared',
            delete = del || del'
        } 

data ScopedModification = ScopedModification {
    key :: SystemKey,
    context :: Context,
    modified :: Modification
}

instance Perspective ScopedModification where
    getComponent ScopedModification{key=key, modified=Modification{owner=owner}} =
        (!) owner key

    alterComponent sm@ScopedModification{key=key, modified=modified@Modification{owner=owner}} alterer =
        sm{modified = modified{owner =
            Map.alter (alterer . fromJust) key owner
        }}
    
    setComponent sm@ScopedModification{key=key, modified=modified@Modification{owner=owner}} comp =
        sm{modified = modified{owner =
            Map.insert key comp owner
        }}
    
    -- Context
    getContext ScopedModification{key=key, context=ctxt} = 
        (!) ctxt key
    
    alterContext sm@ScopedModification{key=key, context=ctxt} alterer =
        sm{context = 
            Map.alter (alterer . fromJust) key ctxt
        }
    
    setContext sm@ScopedModification{key=key, context=ctxt} comp =
        sm{context = 
            Map.insert key comp ctxt
        }
    
    -- Modification to Owner
    setToDelete sm@ScopedModification{modified=modified} =
        sm{modified = modified{delete = True}}

    -- Sharing (is caring ;) )
    share sm@ScopedModification{modified=modified@Modification{shared=shared}} key comp =
        sm{modified = modified{shared =
            Map.insert key comp shared
        }}

    receive ScopedModification{modified=Modification{shared=shared}} key =
        (!) shared key
    
    addIO sm io = addIOs sm [io]

    addIOs sm@ScopedModification{modified=modified@Modification{io=io}} io' = 
        sm{modified=modified{io = io ++ io'}}

    addEntity sm e = addEntities sm [e]

    addEntities sm@ScopedModification{modified=modified@Modification{added=added}} added' = 
        sm{modified=modified{added = added ++ added'}}


-- TODO Modifications should include multiple entities for MultiInput systems.
-- Contexts should be the same across Entity Runs.
scopeModification :: SystemKey -> Context -> Modification -> ScopedModification
scopeModification key ctxt mod = ScopedModification {
    key = key,
    context = ctxt,
    modified = mod
}

maybeScopeModificaiton :: SystemKey -> Context -> Modification -> Maybe ScopedModification
maybeScopeModificaiton key ctxt mod@Modification{owner=owner} 
    | Map.member key owner = Just (scopeModification key ctxt mod)
    | otherwise = Nothing                                                            

-- It is assumed that the system output has to do with the same entity as the 
-- Modification. Thus the modified entity stays the same through the frame.
applyOutputToFold :: SystemKey -> Maybe ScopedModification -> Modification -> Modification
applyOutputToFold _ Nothing fold = fold
applyOutputToFold key (Just ScopedModification {modified = mod}) modFold = merge modFold mod

applyOutputToFolds :: SystemKey -> [Maybe ScopedModification] -> [Modification] -> [Modification]
applyOutputToFolds key = zipWith (applyOutputToFold key)

-- I considered having a readonly list of entities here... 
-- but we are doing some major assertion work anyways and the computation
-- seemed to get pretty unoptimized when working with a secondary list outside of Modification
-- Consider: Maybe include readonly entity in SystmeOutputFold datatype.
foldOverModifications :: DependencyTree SystemKey -> (SystemKey -> SystemImpl ScopedModification) -> Context -> [Modification] -> [Modification]
foldOverModifications deps sysGetter ctxt sysFolds = DependencyTree.foldr' (folder ctxt) sysFolds deps
    where
        folder ctxt' key sysFolds' = applyOutputToFolds key (getOutput key (map (maybeScopeModificaiton key ctxt') sysFolds')) sysFolds'
        getOutput key' = runSystem (sysGetter key')

runSystem :: Perspective a => SystemImpl a -> [Maybe a] -> [Maybe a]
runSystem (SINGLE sys) = map (runSingleSystem sys)
runSystem (BATCH filter sys) = runMultiSystem filter sys
runSystem (ALL sys) = runMultiSystem (const (Just 0)) sys

runSingleSystem :: Perspective a => SingleInputSystem a -> Maybe a -> Maybe a
runSingleSystem sys = defaultNothing (Just . sys)

runMultiSystem :: Perspective a => (a -> Priority) -> MultiInputSystem a -> [Maybe a] -> [Maybe a]
runMultiSystem filter sys maybeInputs = keepOrder outputs maybeInputs
    where
        inputs = catMaybes maybeInputs
        maybeBatches = map filter inputs
        batches = catMaybes maybeBatches
        mapBatchToInputs = foldr folder Map.empty (zip batches inputs)
        folder (num, input) = Map.alter (Just . (:) input . fromMaybe []) num
        mapBatchToOutputs = Map.map sys mapBatchToInputs
        outputs = batchToInOrder mapBatchToOutputs (reverse maybeBatches)

batchToInOrder :: Perspective a => Map.Map Int [Maybe a] -> [Priority] -> [Maybe a]
batchToInOrder m [] | Map.null m = []
                    | otherwise = error "System Provided More Outputs than Inputs"
batchToInOrder m (Nothing : xs) = Nothing : batchToInOrder m xs
batchToInOrder m ((Just x) : xs)
                    | Map.member x m && not (null lst) = head lst : batchToInOrder (Map.insert x (tail lst) m) xs
                    | otherwise = error "Engine Mapping Error"
    where lst = (!) m x


-- Assumes first list is in the same order as the second
keepOrder :: Perspective a => [Maybe a] -> [Maybe a] -> [Maybe a]
keepOrder [] [] = []
keepOrder [] ((Just y) : ys) = error "System Gave Smaller Number of Outputs"
keepOrder (x : xs) [] = error "System Gave Larger Number of Outputs"
keepOrder (x : xs) ((Just y) : ys) = x : keepOrder xs ys
keepOrder lst (Nothing : ys) = Nothing : keepOrder lst ys

-- TODO Add Context
outputsToNewFrame :: [Modification] -> ([IO ()], [Entity])
outputsToNewFrame = foldr outputToNewFrame ([], [])

outputToNewFrame :: Modification -> ([IO ()], [Entity]) -> ([IO ()], [Entity])
outputToNewFrame Modification{io=ioj, added=addedj, owner=owner, delete=False} (ios, es) = (ios ++ ioj, owner : addedj ++ es)
outputToNewFrame Modification{io=ioj, added=addedj, owner=owner, delete=True} (ios, es) = (ios ++ ioj, addedj ++ es)
