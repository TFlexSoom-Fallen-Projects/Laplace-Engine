{-|
    =__Entity:__
    Data Holder
    Holds the attached/acting Systems on the piece of data represented through Map's key
    Holds possible callstack of previous System Actions (Component)
-}
module Core.Entity (
    Entity,
    addComponent,
    addComponentWith,
    newEntityFromList,
    getComponent,
    singletonEntity,
) where

import Data.Map((!))
import qualified Data.Map as Map

import Core.Component(Component)
import Core.SystemKey (SystemKey)

type Entity = Map.Map SystemKey Component

-- Private
getComponent :: SystemKey -> Entity -> Component
getComponent k e = (!) e k

addComponentWith :: (Component -> Component -> Component)
    -> SystemKey -> Component -> Entity -> Entity
addComponentWith = Map.insertWith

addComponent :: SystemKey -> Component -> Entity -> Entity
addComponent = addComponentWith const

-- Private
addComponentAssert :: SystemKey -> Component -> Entity -> Entity
addComponentAssert = addComponentWith (error "Key Collision Insert")

-- Private
getMaybeComponent :: SystemKey -> Entity -> Maybe Component
getMaybeComponent = Map.lookup

newEntityFromList :: [Entity -> Entity] -> Entity
newEntityFromList = foldl (\ arg x -> x arg) Map.empty

singletonEntity :: SystemKey -> Component -> Entity
singletonEntity k c = Map.insert k c Map.empty