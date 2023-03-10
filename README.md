# Haskell-Intro-to-Computation
A collection of Haskell tasks marking the beginning of my Higher Education in Computer Science

## Installation
The haskell compiler should be installed from the GHCup website https://www.haskell.org/ghcup/ where there are installation instructions for all common operating systems. All optional additional packages should be added when prompted by the installation procedure.

## Running
Each file contains solutions to problem sets. The 'Tutorials' directory contains solutions to the  tutorials shown on https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/#haskell. The 'Test Practice' directory contains attempted solutions to past paper questions as directed from the aforementioned page, https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/exams/

The functions in each file can be used by first running ghci \<name of file\>. Once inside the ghci interactive environment, any given function, f with given arguments, x, y, z etc can be used as: ```f x y z```

# QuickCheck Testing
The QuickCheck module can be installed by running the following command in the root directory:
```
cabal install --lib QuickCheck
```

The cabal tool should already be installed from GHCup.

Testing variables can be run by calling `quickCheck` and then the variable or function. There is no consistent naming policy for these testing variables but they will usually begin with "prop" or in some cases a "t".
