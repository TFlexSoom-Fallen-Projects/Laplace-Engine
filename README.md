# Laplace-Engine
An attempt at a Game Engine made entirely in Haskell. A hobby project of sorts with no real deadline. Currently an architecture is outlined but not implemented.

## Repository Structure
Structure Modelled off of: [Haskelleton](http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/)

# Current Features
* Print Hello World to the Console

# Build From Source
0. Install the [The Cabal Build System with GHC](https://www.haskell.org/downloads/#platform)
1. `git clone https://github.com/TFlexSoom/Laplace-Engine`
2. `stack build`
3. `stack exec Laplace-Engine-exec`

# How to Use Project
Currently, the project is not finished, however, the goal is to create games with minimal haskell code and a documented API. This is done through an Entity Component System similar to that in other game engines. Under `game` there is a module called `ExampleGame.hs` which represents how a new game is created. (In the future, it may be better to create a `.ini` or `.json` file to carry this data instead.) The goal is to use the data placed in this format to create the entire game.

# How to (Unit) Test
0. Install the [The Cabal Build System with GHC](https://www.haskell.org/downloads/#platform)
1. `git clone https://github.com/TFlexSoom/Laplace-Engine`
2. `stack test`

# Contact
* Tristan Hilbert
* tchilbert\[at\]live\[dot\]com

# License
MIT
