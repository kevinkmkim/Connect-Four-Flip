# Connect 4 Flip
Course Project for CS3110 Data Structures and Functional Programming 
### Authors
- Eric Zhang (ezz5)
- Kevin Kim (kk946)
- Matt Salud (mls569)
- Pun Chaixanien (sc2343)

## Introduction
This project is a text based game called 'Connect 4 Flip', a variant of Connect 4. The difference between Connect 4 is that the player can use a turn to flip the board by 180 degrees instead of placing a tile. This will cause gravity to pull all the tiles down and alter the board state. A player wins when they have more unique connect fours on the board at a single point.

## Instruction
Run `make play` to play the game.

## Useful `make` Commands
- `make build` runs `dune build`
- `make test` runs the test suite
- `make play` runs the game
- `make zip` creates a zip file of the program
- `make check_lines` shows the total lines of code in the program
    - run `brew install cloc` in case of `cloc: command not found` error