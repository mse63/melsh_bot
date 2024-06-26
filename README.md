# melsh_bot
A UCI chess engine.
You can play against melsh_bot on [lichess](https://lichess.org/@/melsh_bot)

## Overview

melsh_bot is a Rust UCI chess engine created by Mahmoud Elsharawy that analyzes chess positions and computes the optimal moves.

melsh_bot is compatable with UCI graphical user interfaces which display a chessboard and to make it easy to input moves, such as CuteChess and Arena.
Read the documentation for your GUI of choice for information about how to use compiled binaries with it.

## Files

This repository consists of the following files:

  * README.md - the file you are currently reading.

  * src - a subdirectory containing the full source code.

  * Cargo.toml - a cargo file describing the Rust package for ease of compilation.

## Compiling melsh_bot

melsh_bot is most easily compiled with Cargo.

On Unix-like systems, melsh_bot can be compiled with: `cargo build --release`
