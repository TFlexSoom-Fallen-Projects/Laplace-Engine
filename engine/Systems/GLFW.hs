module Systems.Input (
    newGLFW,
    addGLFW,
    enableGLFW
) where

import Data.Map(findWithDefault)
import Graphics.UI.GLFW ( Window )
import Engine (
    SystemKey,
    enableSystem,
    Entity,
    System,
    concatIO,
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

addGLFW :: Entity -> Entity 
addGLFW = adjustDefaultComponent glfwKey [METADATA "GLFW Context" (COMPONENT glfw)] glfwDefault

enableGLFW :: Game -> Game 
enableGLFW = enableSystem glfwKey

-- Implmentation

glfwDefault :: [Component]
glfwDefault = [METADATA "GLFW System Base" (COMPONENT glfwBase)]

-- TODO , Don't concat IO directly for this system
glfwBase :: System
glfwBase entity = (concatIO (tail components) entity, entity)
    where components = findWithDefault [] glfwKey entity 

-- TODO Fix
glfw :: System
glfw entity = ([], entity)