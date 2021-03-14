-- | Module Definition for Laplace-Engine
module Engine (
    SystemKey,
    Entity,
    System,
    Component(..),
    Game,
    newGame,
    newEntity,
    run
) where

import Data.Map (Map, empty)
import qualified Data.Map as Map

type SystemKey = String

type Entity = Map SystemKey [Component]

type System = Entity -> Entity

type Game = [Entity]

data Component = Tail System | Frame System Component

newGame :: Game
newGame = []

newEntity :: Entity
newEntity = empty

frame :: [System] -> Game -> Game
frame (x:xs) game = frame xs (map x game) 
frame [] game = game

run :: [System] -> Game -> [System] -> Game
run keys game = run keys (frame keys game)