{-# LANGUAGE DuplicateRecordFields #-}
{-
    =__System:__
    Acting Agent
    Takes a Data Holder, performs work on it and possibly adding to a 
    stack on instructions sent to the operating system

    ==__Laws:__
    1. Every System should have a SystemKey
    2. (len input) == (len output)
-}
module Core.System (
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

) where

import Data.Map((!))
import qualified Data.Map as Map

import Core.SystemKey (SystemKey)
import Core.Component (Component(..))
import Core.Entity (Entity)
import Core.Util (Creatable(..), Mergeable(..), mergeUnsafe, apply, defaultNothing, assert)

type SingleInputSystem = SystemInput -> SystemOutput
type MultiInputSystem = [SystemInput] -> [Maybe SystemOutput]

data System = SINGLE SingleInputSystem
    --                        V Can be any Ord instance
    | BATCH  (SystemInput -> Maybe Int) MultiInputSystem
    | ALL                               MultiInputSystem


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
    newShares :: ShareMap
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