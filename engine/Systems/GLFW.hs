module Systems.GLFW (
    newGLFW,
    addGLFW,
    enableGLFW
) where

import Data.Map(findWithDefault)
import Graphics.UI.GLFW ( Window )
import Dynamic (Dynamic, DynamicallyAware(..), DynamicHolder(..))
import Engine (
    SystemKey,
    enableSystem,
    Entity,
    System,
    SystemOutput(..),
    Game,
    Component(..),
    insertComponent,
    insertComponents,
    adjustDefaultComponent,
    adjustComponent)

-- | System to translate Actions to Window Contexts. Depdenency for a lot of other systems.

-- | Key Function Required By All Systems
glfwKey :: SystemKey
glfwKey = "glfwSys"

-- Interface

newGLFW :: Entity -> Entity
newGLFW = insertComponent glfwKey glfwDefault

-- TODO Fix
addGLFW :: Entity -> Entity 
addGLFW = adjustDefaultComponent glfwKey [] glfwDefault

enableGLFW :: Game -> Game 
enableGLFW = enableSystem glfwKey glfw

-- Implmentation

glfwDefault :: [Component]
glfwDefault = [VALUE (toDyn "Hello World")]

-- TODO Fix
glfw :: System
glfw comps = SystemOutput {
    io = [],
    entity = comps,
    new = []
}