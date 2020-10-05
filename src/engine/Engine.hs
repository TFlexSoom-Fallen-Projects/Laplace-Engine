-- | Module Definition for Laplace-Engine
module Engine (
-- Types
    Game,
    Entity,
-- Methods
    getName,
    addComponentToEntity,
-- Functions
    engine,
    newEntity,
    newText
) where

import Data.Map (Map, insert, keys, empty)


-- Base Type definitions
data Entry = NONE
    | TPL Entry Entry
    | STR [Char]
    | INT Integer
    | FLT Float

type Component = (String, [Entry])
type Systems = ([Entity], Entity) -> ([Entity], Entity)
type Entity = Map [Char] Component
type Game = [Entity]

-- Getting the name of a component
getName :: Component -> String
getName x = fst x

-- Create a blank entity
newEntity :: Entity
newEntity = empty

-- Add an entity to a game
addComponentToEntity :: (Component, Entity) -> Entity
addComponentToEntity (a, b) = insert (getName a) a b

-- Revving the engine
engine :: Game -> IO ()
engine [] = print "Done"
engine (ent:ents) = do{print (keys ent);  engine (ents)}

-- Text Definition Example
-- Text should also have coordinates among other properties
newText :: [Char] -> Component
newText s = ("Text", [STR s])