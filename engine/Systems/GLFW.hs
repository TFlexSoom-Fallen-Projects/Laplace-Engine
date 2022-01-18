module Systems.GLFW (
    newGLFW,
    addGLFW,
    enableGLFW
) where

import qualified Data.Map as Map
import Graphics.UI.GLFW ( Window )
import Core.Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Core.Component(Component(..))
import Core.Entity(Entity, addComponent)
import Core.Game(Game, enableSystem)
import Core.System (
    MultiInputSystem,
    System(..),
    SystemKey,
    ShareMap,
    SystemInput(..),
    SystemOutput(..),
    Modification(..),
    EngineJob(..),
    )

-- | System to translate Actions to Window Contexts. Depdenency for a lot of other systems.

-- | Key Function Required By All Systems
glfwKey :: SystemKey
glfwKey = "glfwSys"

-- Interface

newGLFW :: Entity -> Entity
newGLFW = addComponent glfwKey glfwDefault

-- TODO Fix
addGLFW :: Entity -> Entity 
addGLFW = newGLFW

enableGLFW :: Game -> Game 
enableGLFW = enableSystem glfwKey glfw

-- Implmentation

glfwDefault :: Component
glfwDefault = VALUE (toDyn "derp Depr Derp")

cast :: Component -> String
cast (VALUE c) = fromDyn c
cast _         = ""

glfw :: System
glfw = ALL glfwImpl

glfwImpl :: MultiInputSystem
glfwImpl = map (Just . glfwImplSingle)

glfwImplSingle :: SystemInput -> SystemOutput
glfwImplSingle SystemInput {component=comp} = SystemOutput {
    modification = Modification {
            modified = comp,
            delete = False,
            newShares = Map.empty
        },
    job = EngineJob {
        io = [putStrLn (cast comp)],
        added = []
        }
}