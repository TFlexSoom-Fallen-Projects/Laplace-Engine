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

-- class Implementations --
-----------------------------------------------------------------------------------
-- |
--   FramePerspective: Store Everything in a Tuple
data SystemImpl = SystemImpl
  {
    key :: String,
    implementation :: FramePerspectiveImpl -> FramePerspectiveImpl,
    contextSupplier :: () -> Component
  }

fromSystemData :: System a => a -> SystemImpl
fromSystemData sys = SystemImpl 
  {
    key = getKey sys,
    implementation = getImplementation sys,
    contextSupplier = \ _ -> initContext sys
  }

-----------------------------------------------------------------------------------
-- |
--   Game Implementation: Store everything in a Tuple
data GameImpl = GameImpl
  { systems :: Map.Map SystemKey SystemImpl,
    dependency :: DependencyTree.DependencyTree SystemKey,
    context :: Context,
    entities :: [Entity]
  }

type Context = Entity

gameV1 :: GameImpl
gameV1 = new :: GameImpl

replaceSystems :: Map.Map SystemKey SystemImpl -> GameImpl -> GameImpl
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
      frameFunc k = newFrame k ((context :: GameImpl -> Context) g) es 
      sysFolds = transformFrame dep (sysMap !) frameFunc
      result = outputsToNewFrame sysFolds

transformFrame :: DependencyTree SystemKey -> (SystemKey -> SystemImpl) -> (SystemKey -> FramePerspectiveImpl) -> FramePerspectiveImpl
transformFrame deps sysGetter frameFunc = DependencyTree.foldr' foldWithKeys emptyFrame deps
  where 
    emptyFrame = frameFunc ""
    foldWithKeys key = runSystem (sysImpl key) . replaceKey key
    sysImpl key = implementation (sysGetter key)

runSystem :: FramePerspective a => a -> a
runSystem sys = sys


outputsToNewFrame :: FramePerspectiveImpl -> ([IO ()], [Entity])
outputsToNewFrame FramePerspectiveImpl {io = io} = (io, [])

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
      sysCtxt = initContext sys
      newSystems = Map.insert key sys sysMap
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

-----------------------------------------------------------------------------------
-- |
--   EntityPerspective: Store Everything in a Tuple
data EntityPerspectiveImpl = EntityPerspectiveImpl
  { key :: Maybe SystemKey,
    owner :: Entity,
    shared :: ShareMap,
    delete :: Bool
  }

type ShareMap = Map.Map SharingKey Component

newEntityPerspectiveImpl :: Entity -> EntityPerspectiveImpl
newEntityPerspectiveImpl e =
  EntityPerspectiveImpl
    { key = Nothing,
      owner = e,
      shared = Map.empty,
      delete = False
    }

newEntityPerspectiveImpls :: [Entity] -> [EntityPerspectiveImpl]
newEntityPerspectiveImpls = map newEntityPerspectiveImpl

instance Mergeable EntityPerspectiveImpl where
  merge
    mod@EntityPerspectiveImpl {owner = owner, shared = shared, delete = del}
    EntityPerspectiveImpl {owner = owner', shared = shared', delete = del'} =
      mod
        { key = Nothing,
          owner = Map.union owner owner',
          shared = Map.union shared shared',
          delete = del || del'
        }

instance EntityPerspective EntityPerspectiveImpl where
  setKey k impl = impl {key = Just k}

  getComponent EntityPerspectiveImpl {key = (Just k), owner = owner} =
    (!) owner k
  getComponent EntityPerspectiveImpl {key = Nothing} = error "Key Not Set"

  setComponent impl@EntityPerspectiveImpl {key = (Just k), owner = owner} comp =
    impl {owner = Map.insert k comp owner}
  setComponent EntityPerspectiveImpl {key = Nothing} _ = error "Key Not Set"

  alterComponent impl@EntityPerspectiveImpl {key = (Just k), owner = owner} alteration =
    impl {owner = Map.alter alteration k owner}
  alterComponent EntityPerspectiveImpl {key = Nothing} _ = error "Key Not Set"

  deleteComponent impl@EntityPerspectiveImpl {key = (Just k), owner = owner} =
    impl {owner = Map.delete k owner}
  deleteComponent EntityPerspectiveImpl {key = Nothing} = error "Key Not Set"

  share impl@EntityPerspectiveImpl {shared = shared} k comp =
    impl {shared = Map.insert k comp shared}

  receive EntityPerspectiveImpl {shared = shared} k =
    Map.lookup k shared

  setToDelete impl = impl {delete = True}

-----------------------------------------------------------------------------------
-- |
--   FramePerspective: Store Everything in a Tuple
data FramePerspectiveImpl = FramePerspectiveImpl
  { key :: SystemKey,
    io :: [IO ()],
    add :: [Entity],
    context :: Context,
    mods :: [EntityPerspectiveImpl]
  }

newFrame :: SystemKey -> Context -> [Entity] -> FramePerspectiveImpl
newFrame key ctxt es =
  FramePerspectiveImpl
    { key = key,
      io = [],
      add = [],
      context = ctxt,
      mods = newEntityPerspectiveImpls es
    }

replaceKey :: SystemKey -> FramePerspectiveImpl -> FramePerspectiveImpl
replaceKey key impl = impl {key = key}

instance Mergeable FramePerspectiveImpl where
  merge
    mod@FramePerspectiveImpl {io = io, add = add, context = ctxt, mods = mods}
    FramePerspectiveImpl {io = io', add = add', context = ctxt', mods = mods'} =
      mod
        { -- Overwrite key with left key
          io = io ++ io',
          add = add ++ add',
          context = Map.union ctxt ctxt',
          mods = zipWith merge mods mods'
        }

instance FramePerspective FramePerspectiveImpl where
  -- Modifications to Entity
  alterEntities impl@FramePerspectiveImpl {mods = ms} alteration =
    impl {mods = alteration ms}

  -- Modifications to Context
  getContext FramePerspectiveImpl {key = k, context = ctxt} = (!) ctxt k
  alterContext impl@FramePerspectiveImpl {key = k, context = ctxt} alteration =
    impl {context = Map.alter alteration k ctxt}
  setContext impl@FramePerspectiveImpl {key = k, context = ctxt} comp =
    impl {context = Map.insert k comp ctxt}

  -- IO
  addIO impl io = addIOs impl [io]
  addIOs impl@FramePerspectiveImpl {io = ios} ios' =
    impl {io = ios ++ ios'}

  -- Entities
  addEntity impl e = addEntities impl [e]
  addEntities impl@FramePerspectiveImpl {add = es} es' =
    impl {add = es ++ es'}