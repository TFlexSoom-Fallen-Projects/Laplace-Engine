module Systems.Console
  ( console,
    addMessage,
  )
where

import Core.Component (Component (..))
import Core.Dynamic (Dynamic, DynamicHolder (..), DynamicallyAware (..))
import Core.Entity (Entity, addComponent)
import Core.System
  ( EntityPerspective (..),
    FramePerspective (..),
    System (..),
  )
import Core.SystemKey (SystemKey)

newtype Console = Console ()

instance System Console where
  getKey _ = consoleKey

  getImplementation _ = consoleImpl

  initContext _ = VALUE (toDyn ())

  initComponent _ = consoleDefault

console :: Console
console = Console ()

consoleKey :: SystemKey
consoleKey = "ConSys"

addMessage :: String -> Entity -> Entity
addMessage msg = addComponent consoleKey (VALUE (toDyn msg))

-- Implementation

consoleDefault :: Component
consoleDefault = VALUE (toDyn "Hello World")

cast :: Component -> String
cast (VALUE c) = fromDyn c
cast _ = ""

consoleImpl :: (FramePerspective f, EntityPerspective e) => (f, [e]) -> (f, [e])
consoleImpl (f, es) = (foldr (\e frame -> addIO frame (putStrLn (cast (getComponent e)))) f es, es)