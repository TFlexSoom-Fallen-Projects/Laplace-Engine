{-|
    =__Game:__
    List of entities which can be filtered with system system key map. Entities will then
    used their captured lambdas to perform the work of the game.
-}
module Core.Game (
    Game(..),
) where

import Core.SystemKey (SystemKey)
import Core.Component (Component(..))
import Core.Entity (Entity)
import Core.System (System)

class Game a where 
    enableSystem :: System b => b -> a -> a
    addEntity :: Entity -> a -> a
    runFrames :: Int -> a -> IO ()
    runGame :: a -> IO ()