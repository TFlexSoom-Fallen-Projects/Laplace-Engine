module Systems.GLFW (
    GLFW
) where

import Core.Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Core.SystemKey (SystemKey)
import Core.Component(Component(..))
import Core.Entity(Entity)
import Core.System (
    SharingKey,
    Perspective(..),
    MultiInputSystem,
    SystemImpl(..),
    System(..)
    )

newtype GLFW = GLFW ()

instance System GLFW where
    getKey _ = glfwKey

    getImplementation _ = ALL glfw

    initContext _ = VALUE (toDyn ())

    initComponent _ = glfwDefault

-- | System to translate Actions to Window Contexts. Depdenency for a lot of other systems.

-- | Key Function Required By All Systems
glfwKey :: SystemKey
glfwKey = "glfwSys"

-- Implmentation

glfwDefault :: Component
glfwDefault = VALUE (toDyn "derp Depr Derp")

cast :: Component -> String
cast (VALUE c) = fromDyn c
cast _         = ""

glfw :: Perspective a => MultiInputSystem a
glfw = map (Just . glfwIter)

glfwIter :: Perspective a => a -> a
glfwIter modif = addIO modif ((putStrLn . (cast . getComponent)) modif)