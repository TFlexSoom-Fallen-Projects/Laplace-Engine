{-| =__Utility:__
    Utilities for help in defining structures and functionality for the engine.
    The goal of these is to avoid repeating myself. As I coded the engine
    I found myself chasing typical patterns. I decided to classify these.

    This may include some additional classes and assertion utilities in the future
-}
module Core.Util where

import qualified Data.Map as Map

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

apply :: a -> (a -> b) -> b
apply a lambda = lambda a

defaultNothing :: (a -> Maybe b) -> Maybe a -> Maybe b
defaultNothing = maybe Nothing

assert :: [Char] -> (a -> Bool) -> a -> a
assert err lambda a | lambda a = a
                    | otherwise = error err