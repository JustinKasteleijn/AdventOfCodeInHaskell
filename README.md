# Advent of Code in Haskell 

This project will contain all of my solutions for Advent of Code (AoC).

## Command 

`cabal run AdventOfCodeInHaskell -- -year 2015 -day 1`

## Comments on Days 

### Year 2015 

#### Day 6 

- Use `STUArray` for a mutable grid for performance.
- Flatten the 2D grid into a 1D array using **row-major order**:
