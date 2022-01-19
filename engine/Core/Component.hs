{-|
    =__Component:__
    Dynamic Data for a entity. Each System has it's own version of a DynamicHolder representing a collection
    of data. Think like fields on a Java Object. These fields of the instance are carried under the tree of data.
    Since the types of the data are hidden from the engine, the engine trusts the systems to be responsible for 
    handling all cases of the data. Poor casts should result in static errors for Engine development and runtime 
    errors for game development
-}
module Core.Component (
    Component(..),
) where

-- Rewritten Dynamic Wheel for Heterogeneous lists
import Core.Dynamic (Dynamic, DynamicallyAware, DynamicHolder)

type Component = DynamicHolder


