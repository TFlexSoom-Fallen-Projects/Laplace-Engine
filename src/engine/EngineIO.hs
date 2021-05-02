-- | Module Definition for IO within Laplace-Engine
module EngineIO (
  performActions
) where

import Engine (Action(..))

{-
  EngineIO 
  takes all of the actions for engine and maps it to an IO Monad Operation
-}

performActions :: [Action] -> [IO ()]
performActions actions = map perf actions

perf :: Action -> IO ()
perf (LOG msg) = putStrLn msg