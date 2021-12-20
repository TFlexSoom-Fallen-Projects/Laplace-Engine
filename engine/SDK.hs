module SDK (
    newEntity,
    newEntityFromList,
    addEntity,
    Game,
    newGame,
    runFrame,
    runGame
) where

-- | Used to Isolate individaul functions from The Engine as a public interface.
import Engine(
    newEntity,
    newEntityFromList,
    addEntity,
    Game,
    newGame,
    runFrame,
    runGame)