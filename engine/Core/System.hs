{-
    =__System:__
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a
    stack on instructions sent to the operating system

    ==__Laws:__
    1. Every System should have a SystemKey
-}
module Core.System
  ( -- For Engine Use Only
    Priority,
    SharingKey,
    EntityPerspective (..),
    FramePerspective (..),
    System (..),
    defaultPriority,
  )
where

import Core.Component (Component (..))
import Core.Entity (Entity)
import Core.SystemKey (SystemKey)

--    V Can be any Ord instance
type Priority = Maybe Int

type SharingKey = String

defaultPriority :: Priority
defaultPriority = Just 1

-- TODO Keys would be better as 64 bit integers than strings

class EntityPerspective e where
  setKey :: SystemKey -> e -> e

  getComponent :: e -> Component
  setComponent :: e -> Component -> e
  alterComponent :: e -> (Maybe Component -> Maybe Component) -> e
  deleteComponent :: e -> e

  share :: e -> SharingKey -> Component -> e
  receive :: e -> SharingKey -> Maybe Component

  setToDelete :: e -> e

class FramePerspective f where
  -- Modifications to Context
  getContext :: f -> Component
  alterContext :: f -> (Maybe Component -> Maybe Component) -> f
  setContext :: f -> Component -> f

  -- IO
  addIO :: f -> IO () -> f
  addIOs :: f -> [IO ()] -> f

  -- Entities
  addEntity :: f -> Entity -> f
  addEntities :: f -> [Entity] -> f

class System a where
  getKey :: a -> SystemKey
  getImplementation :: (FramePerspective f, EntityPerspective e) => a -> (f, [e]) -> (f, [e])
  initContext :: a -> Component
  initComponent :: a -> Component
