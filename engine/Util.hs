module Util(
    concatTplList
) where

import Data.Bifunctor(bimap)

-- | Utility Functions

concatTplList :: [([a], b)] -> ([a], [b])
concatTplList (cur:others) = bimap ( fst cur ++ ) ( [snd cur] ++ ) res
    where res = concatTplList others
concatTplList [] = ([], [])