module Core.SystemKey where

{-|
    ==__SystemKey: __
    Key to access Component on an Entity
    Each System should have it's own unique key to show existance on an entity
-}
type SystemKey = String