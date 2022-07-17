{-# LANGUAGE DuplicateRecordFields #-}

-- |
--    =__Engine:__
--    Implementation of all classes
module Core.Engine
  ( GameImpl,
    gameV1,
  )
where

-- Base Imports

import Core.Component (Component (..))
import Core.DependencyTree (DependencyTree)
import qualified Core.DependencyTree as DependencyTree
import Core.Entity (Entity)
import Core.Game (Game (..))
import Core.System
  ( EntityPerspective (..),
    FramePerspective (..),
    Priority,
    SharingKey,
    System (..),
    defaultPriority,
  )
import Core.SystemKey (SystemKey)
import Core.Util (Creatable (..), Mergeable (..), apply, assert, defaultNothing, mergeUnsafe)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)

-------------------------   IMPLEMENTATION BELOW   ----------------------------

type Context = Entity

data GameImpl = GameImpl
  { systems :: Map.Map SystemKey System,
    dependency :: DependencyTree.DependencyTree SystemKey,
    context :: Context,
    entities :: [Entity]
  }

gameV1 :: GameImpl
gameV1 = new :: GameImpl

replaceSystems :: Map.Map SystemKey System -> GameImpl -> GameImpl
replaceSystems sys g = g {systems = sys}

replaceDependency :: DependencyTree SystemKey -> GameImpl -> GameImpl
replaceDependency dep g = g {dependency = dep}

replaceEntities :: [Entity] -> GameImpl -> GameImpl
replaceEntities e g = g {entities = e}

replaceContext :: Entity -> GameImpl -> GameImpl
replaceContext e g = g {context = e}

runFrame :: GameImpl -> ([IO ()], GameImpl)
runFrame
  g@GameImpl
    { systems = sysMap,
      dependency = dep,
      entities = es
    } = (fst result, replaceEntities (snd result) g)
    where
      frame = newFrame ((context :: GameImpl -> Context) g) es
      sysFolds = foldOverEntityFrames dep (sysMap !) frame
      result = outputsToNewFrame sysFolds

instance Creatable GameImpl where
  new =
    GameImpl
      { systems = Map.empty,
        dependency = DependencyTree.empty,
        context = Map.empty,
        entities = []
      }

instance Mergeable GameImpl where
  merge g g' =
    g
      { systems = mergeUnsafe (systems g) (systems g'),
        dependency = DependencyTree.union (dependency g) (dependency g'),
        context = mergeUnsafe ((context :: GameImpl -> Context) g) ((context :: GameImpl -> Context) g'),
        entities = entities g ++ entities g'
      }

instance Game GameImpl where
  enable sys g@GameImpl {systems = sysMap, dependency = deps, context = ctxt} =
    replaceSystems newSystems
      . replaceDependency newDependency
      . replaceContext newContext
      $ g
    where
      key = getKey sys
      sysPart = getPartition sys
      sysImpl = getImplementation sys
      sysWork = createSystemWork sysPart sysImpl
      sysCtxt = initContext sys
      newSystems = Map.insert key sysWork sysMap
      newDependency = DependencyTree.insert key deps
      newContext = Map.insert key sysCtxt ctxt

  addEntity entity g = replaceEntities (entities g ++ [entity]) g

  runFrames 0 _ = pure ()
  runFrames num game = do
    (io, modifiedGame) <- return (runFrame game)
    foldr (>>) (pure ()) io
    runFrames (num - 1) modifiedGame

  runGame game = do
    (io, modifiedGame) <- return (runFrame game)
    foldr (>>) (pure ()) io
    runGame modifiedGame

type ShareMap = Map.Map SharingKey Component

-- Private
data EntityModification = EntityModification
  { key :: Maybe SystemKey,
    owner :: Entity,
    shared :: ShareMap,
    delete :: Bool
  }

newEntityModification :: Entity -> EntityModification
newEntityModification e =
  EntityModification
    { key = Nothing,
      owner = e,
      shared = Map.empty,
      delete = False
    }

newEntityModifications :: [Entity] -> [EntityModification]
newEntityModifications = map newEntityModification

instance Mergeable EntityModification where
  merge
    mod@EntityModification {owner = owner, shared = shared, delete = del}
    EntityModification {owner = owner', shared = shared', delete = del'} =
      mod
        { key = Nothing,
          owner = Map.union owner owner',
          shared = Map.union shared shared',
          delete = del || del'
        }

instance EntityPerspective EntityModification where
  setKey k impl = impl {key = Just k}

  getComponent EntityModification {key = (Just k), owner = owner} =
    (!) owner k
  getComponent EntityModification {key = Nothing} = error "Key Not Set"

  setComponent impl@EntityModification {key = (Just k), owner = owner} comp =
    impl {owner = Map.insert k comp owner}
  setComponent EntityModification {key = Nothing} _ = error "Key Not Set"

  alterComponent impl@EntityModification {key = (Just k), owner = owner} alteration =
    impl {owner = Map.alter alteration k owner}
  alterComponent EntityModification {key = Nothing} _ = error "Key Not Set"

  deleteComponent impl@EntityModification {key = (Just k), owner = owner} =
    impl {owner = Map.delete k owner}
  deleteComponent EntityModification {key = Nothing} = error "Key Not Set"

  share impl@EntityModification {shared = shared} k comp =
    impl {shared = Map.insert k comp shared}

  receive EntityModification {shared = shared} k =
    Map.lookup k shared

  setToDelete impl = impl {delete = True}

data Frame = Frame
  { io :: [IO ()],
    add :: [Entity],
    context :: Context,
    mods :: [EntityModification]
  }

newFrame :: Context -> [Entity] -> Frame
newFrame ctxt es =
  Frame
    { io = [],
      add = [],
      context = ctxt,
      mods = newEntityModifications es
    }

instance Mergeable Frame where
  merge
    mod@Frame {io = io, add = add, context = ctxt, mods = mods}
    Frame {io = io', add = add', context = ctxt', mods = mods'} =
      mod
        { io = io ++ io',
          add = add ++ add',
          context = Map.union ctxt ctxt',
          mods = zipWith merge mods mods'
        }

data FramePerspectiveImpl = FramePerspectiveImpl
  { key :: SystemKey,
    frame :: Frame
  }

newFramePerspective :: SystemKey -> Frame -> FramePerspectiveImpl
newFramePerspective key f =
  FramePerspectiveImpl
    { key = key,
      frame = f
    }

instance FramePerspective FramePerspectiveImpl where
  -- Modifications to Entity
  alterEntities impl@FramePerspectiveImpl {key = k, frame = frame@Frame {mods = ms}} alteration =
    impl {frame = frame {mods = map (setKey k) ms}}

  -- Modifications to Context
  getContext FramePerspectiveImpl {key = k, frame = Frame {context = ctxt}} = (!) ctxt k
  alterContext impl@FramePerspectiveImpl {key = k, frame = frame@Frame {context = ctxt}} alteration =
    impl {frame = frame {context = Map.alter alteration k ctxt}}
  setContext impl@FramePerspectiveImpl {key = k, frame = frame@Frame {context = ctxt}} comp =
    impl {frame = frame {context = Map.insert k comp ctxt}}

  -- IO
  addIO impl io = addIOs impl [io]
  addIOs impl@FramePerspectiveImpl {frame = frame@Frame {io = ios}} ios' =
    impl {frame = frame {io = ios ++ ios'}}

  -- Entities
  addEntity impl e = addEntities impl [e]
  addEntities impl@FramePerspectiveImpl {frame = frame@Frame {add = es}} es' =
    impl {frame = frame {add = es ++ es'}}

transformFrame :: DependencyTree SystemKey -> (SystemKey -> SystemWork) -> Frame -> Frame
transformFrame deps sysGetter frame = DependencyTree.foldr' foldWithKeys frame deps
  where
    foldWithKeys key = foldr' (sysImpl key) (partitionFrame sysPartition frame)
    sysPartition key = (partition :: SystemWork -> (FramePerspectiveImpl -> Priority)) (sys key)
    sysImpl key = (transform :: SystemWork -> (FramePerspectiveImpl -> FramePerspectiveImpl)) (sys key)
    sys key = sysGetter key

runSystem :: FramePerspective a => a -> a
runSystem sys = sys

runMultiSystem :: FramePerspective a => (a -> Priority) -> SystemWork -> [Maybe a] -> [Maybe a]
runMultiSystem filter sys maybeInputs = keepOrder outputs maybeInputs
  where
    inputs = catMaybes maybeInputs
    maybeBatches = map filter inputs
    mapBatchToInputs = foldr folder Map.empty (zip maybeBatches inputs)
    folder (Nothing, input) = id
    folder (Just num, input) = Map.alter (Just . (:) input . fromMaybe []) num
    mapBatchToOutputs = Map.map sys mapBatchToInputs
    outputs = batchToInOrder mapBatchToOutputs (reverse maybeBatches)

batchToInOrder :: FramePerspective a => Map.Map Int [Maybe a] -> [Priority] -> [Maybe a]
batchToInOrder m []
  | Map.null m = []
  | otherwise = error "System Provided More Outputs than Inputs"
batchToInOrder m (Nothing : xs) = Nothing : batchToInOrder m xs
batchToInOrder m ((Just x) : xs)
  | Map.member x m && length lst > 1 = head lst : batchToInOrder (Map.insert x (tail lst) m) xs
  | Map.member x m && not (null lst) = head lst : batchToInOrder (Map.delete x m) xs
  | otherwise = error "Engine Mapping Error"
  where
    lst = (!) m x

-- Assumes first list is in the same order as the second
keepOrder :: FramePerspective a => [Maybe a] -> [Maybe a] -> [Maybe a]
keepOrder [] [] = []
keepOrder [] ((Just y) : ys) = error "System Gave Smaller Number of Outputs"
keepOrder (x : xs) [] = error "System Gave Larger Number of Outputs"
keepOrder (x : xs) ((Just y) : ys) = x : keepOrder xs ys
keepOrder lst (Nothing : ys) = Nothing : keepOrder lst ys

-- TODO
outputsToNewFrame :: Frame -> ([IO ()], [Entity])
outputsToNewFrame f = ([], [])