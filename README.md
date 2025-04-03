# melsh_bot
A UCI chess engine.
You can play against melsh_bot on [lichess](https://lichess.org/@/melsh_bot)

## Overview

melsh_bot is a Rust UCI chess engine created by Mahmoud Elsharawy that analyzes chess positions and computes the optimal moves.

melsh_bot is compatable with UCI graphical user interfaces which display a chessboard and to make it easy to input moves, such as CuteChess and Arena.
Read the documentation for your GUI of choice for information about how to use compiled binaries with it.

## Minimax

melsh_bot uses an alpha-beta pruned minimax algorithm with move-ordering and a hashtable lookup. After a finite depth, it only considers moves which are heuristically more valuable than a "considerable threshold".

For the purpose of move-ordering and pruning, the value of a move is heuristically determined as the difference in the value of the evaluation function as a result of taking that move. 

## Evaluation

Each piece-square combination is assigned some integer number of centipawns. For example, a white knight on c3 might be worth 275 centipawns. The evaluation of a board is just the value of all the pieces the player has, minus the value of all the pieces the opponent has. Note that here "centipawns" are just used as a unit and doesn't necessarily bear any relation to the value of a pawn.

For the purpose of evaluation, melsh_bot considers a Rook that can castle as a different piece than a Rook that cannot castle, with the former being called a Sook. The weights assigned to each piece-square combination are defined in the EvalFile. Each row in the EvalFile defines the values of a specific piece in the following order: Pawn, Knight, Bishop, Rook, Sook, Queen, King.

Each item in the row represents the value in centipawns of that piece on a square, left to right, row by row, from its side to its opponents side. For example, a 280 in the 11th element of the 3rd row means that a white bishop on c2 or a black bishop on c7 are worth 280 centipawns. It is done this way to force color-independence: the evaluation of an equivalent board will be the same regardless of whether the player is white or black.

## UCI Options

melsh_bot has the following options:
  
  * EvalFile - the path to a .csv file containing the weights for the evaluation function (See Evaluation section).
   If an evaluation file is not given, default values are used (equivalent to those in piece_eval.csv).
   
## Files

This repository consists of the following files:

  * README.md - the file you are currently reading.

  * src - a subdirectory containing the full source code.

  * piece_eval.csv - an example EvalFile (see Options section)
  
  * Cargo.toml - a cargo file describing the Rust package for ease of compilation.

## Compiling melsh_bot

melsh_bot is most easily compiled with Cargo.

On Unix-like systems, melsh_bot can be compiled with: `cargo build --release`
