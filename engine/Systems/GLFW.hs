module Systems.GLFW (
    glfw,
    addGraphics,
) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import qualified Data.Map as M
import Data.Maybe(isJust, fromJust)
import Data.Bifunctor(first)
import Control.Monad ( forever )
import System.Exit ( exitSuccess )
import System.Environment ( getArgs)

import Core.Dynamic (Dynamic(..), DynamicallyAware(..), DynamicHolder(..))
import Core.Util(Creatable(..))
import Core.SystemKey (SystemKey)
import Core.Component(Component(..))
import Core.Entity(Entity, addComponent)
import Core.System (
    SharingKey,
    Perspective(..),
    MultiInputSystem,
    SystemImpl(..),
    System(..)
    )

newtype GLFW = GLFW ()

data GLFWInstance = GLFWInstance {
    started :: Bool,
    options :: String
}

instance Creatable GLFWInstance where
    new = GLFWInstance {
        started = False,
        options = []
    }

instance DynamicallyAware GLFWInstance where
    toDyn GLFWInstance{started=started, options=options} = PAIR_TPL (toDyn started, toDyn options)

    fromDyn (PAIR_TPL (started, options)) = GLFWInstance{started = fromDyn started, options = fromDyn options}
    fromDyn _ = error "Type Error"

    tryFromDyn (PAIR_TPL (started, options))
        | isJust maybeStarted && isJust maybeOptions = Just (GLFWInstance{started = fromJust maybeStarted, options = fromJust maybeOptions})
        | otherwise = Nothing
            where
                maybeStarted = tryFromDyn started
                maybeOptions = tryFromDyn options

    tryFromDyn _ = Nothing


instance System GLFW where
    getKey _ = glfwKey

    getImplementation _ = ALL glfwImpl

    initContext _ = VALUE (toDyn (new :: GLFWInstance))

    initComponent _ = glfwDefault

-- | System to translate Actions to Window Contexts. Depdenency for a lot of other systems.


glfw :: GLFW
glfw = GLFW ()

glfwKey :: SystemKey
glfwKey = "glfwSys"

-- Implmentation

addGraphics :: Entity -> Entity
addGraphics = addComponent glfwKey glfwDefault

glfwDefault :: Component
glfwDefault = VALUE (toDyn ())

performOnContext :: Component -> (GLFWInstance -> GLFWInstance) -> Component
performOnContext (VALUE c) lambda | isJust maybeInst = (VALUE . toDyn) (lambda (fromJust maybeInst))
    where maybeInst = tryFromDyn c
performOnContext _ _       = error "Incorrect Component!"

cast :: Component -> GLFWInstance
cast (VALUE c) = fromDyn c
cast _ = error "Incorrect Component"

uncast :: GLFWInstance -> Component
uncast c = VALUE (toDyn c)

glfwImpl :: Perspective a => MultiInputSystem a
glfwImpl [] = error "Tristan Failed in the graphics department TODO"
glfwImpl modifs = do {
        setContext (head modifs) (fst tplIONewContext) : tail modifs;
        addIOs (head modifs) (snd tplIONewContext) : tail modifs;
        map (Just . glfwIter) modifs
    }
    where tplIONewContext = glfwInitComp (getContext (head modifs))



glfwInitComp :: Component -> (Component, [IO ()])
glfwInitComp comp = first uncast result
    where result = glfwInitSwitch (cast comp)

glfwInitSwitch :: GLFWInstance -> (GLFWInstance, [IO ()])
glfwInitSwitch inst@GLFWInstance{started=False} = (inst, [glfwInit])
glfwInitSwitch inst@GLFWInstance{started=True} = (inst, [])

glfwInit :: IO ()
glfwInit = do
    b <- GLFW.init
    log $ "GLFW.init=" ++ show b
    GLFW.defaultWindowHints
    Just win <- GLFW.createWindow 480 480 "GLFW Tutorial" Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowSizeCallback  win (Just resizeWindow)
    GLFW.setKeyCallback         win (Just keyPressed  )
    GLFW.setWindowCloseCallback win (Just shutdown    )
    onDisplay win $ maybe display4Cubes displayPrimitive (Just GL.Triangles)
  where log = putStrLn

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
  do
    GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()

glfwIter :: Perspective a => a -> a
glfwIter modif = modif

onDisplay :: Window -> IO () -> IO ()
onDisplay win io = do
  GL.clear [ColorBuffer]
  io
  GLFW.swapBuffers win

  forever $ do
     GLFW.pollEvents
     onDisplay win io

myPoints :: [(GL.GLfloat,GL.GLfloat,GL.GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

displayPrimitive :: PrimitiveMode -> IO ()
displayPrimitive pm =
  GL.renderPrimitive
  pm $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints

display4Cubes :: IO ()
display4Cubes = do
  let color3f r g b = GL.color $ GL.Color3 r g (b :: GL.GLfloat)
      vertex3f x y z = GL.vertex $ GL.Vertex3 x y (z :: GL.GLfloat)
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f 0.2 0.2 0
    vertex3f 0.2 0 0

    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f 0.2 (-0.2) 0
    vertex3f 0.2 0 0

    color3f 0 0 1
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f (-0.2) (-0.2) 0
    vertex3f (-0.2) 0 0

    color3f 1 0 1
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f (-0.2) 0.2 0
    vertex3f (-0.2) 0 0



