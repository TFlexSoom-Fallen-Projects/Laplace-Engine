module ExampleGame (exampleGame) where

import Entities.Actor (actor)
import SDK
  ( Creatable (..),
    Game (..),
    GameImpl,
    addEntity,
    addToGame,
    gameV1,
    newEntityFromList,
  )
import Systems.Console (console)

{-

Tristan Hilbert
An example Game Built With Engine

-}
exampleGame :: GameImpl
exampleGame =
  addToGame
    [ enable console,
      addEntity actor
    ]
    gameV1