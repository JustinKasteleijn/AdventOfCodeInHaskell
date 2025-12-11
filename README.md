# Advent of Code in Haskell 

This project will contain all of my solutions for Advent of Code (AoC).

## Progress 

### 2015 Progress

Day 1-5:   ⭐⭐ ⭐⭐ ⭐⭐ ⭐⭐ ⭐⭐  
Day 6-10:  ⭐⭐ ✩✩ ✩✩ ✩✩ ✩✩  
Day 11-15: ✩✩ ✩✩ ✩✩ ✩✩ ✩✩  
Day 16-20: ✩✩ ✩✩ ✩✩ ✩✩ ✩✩  
Day 21-25: ✩✩ ✩✩ ✩✩ ✩✩ ✩✩

## 2025 Progress 

Day 1-5:   ⭐⭐ ✩✩ ✩✩ ✩✩ ✩✩  
Day 6-10:  ✩✩ ✩✩ ✩✩ ✩✩ ✩✩  
Day 11-15: ✩✩ ✩✩ ✩✩ ✩✩ ✩✩  
Day 16-20: ✩✩ ✩✩ ✩✩ ✩✩ ✩✩  
Day 21-25: ✩✩ ✩✩ ✩✩ ✩✩ ✩✩


## Command 

`cabal run AdventOfCodeInHaskell -- -year 2015 -day 1`

## Comments on Days 

### Year 2015 

#### Day 6 

- Use `STUArray` for a mutable grid for performance.
- Flatten the 2D grid into a 1D array using **row-major order**:
