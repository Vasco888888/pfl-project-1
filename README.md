# PFL Project 1: Functional Calculator

A purely functional calculator for arithmetic expressions implemented in Haskell.

## Project Structure

- `src/`: Haskell source files.
  - `Calculator.hs`: Main entry point and calculator logic.
  - `Parsing.hs`: Simple parsing library.
- `Makefile`: Build instructions.
- `.gitignore`: Files to be ignored by Git.

## Requirements

- GHC (Glasgow Haskell Compiler)

## How to Build

To compile the project, run:
```bash
make
```
This will create an executable in `bin/calculator`.

## How to Run

To run the calculator:
```bash
make run
```
Or directly:
```bash
./bin/calculator
```

## How to Clean

To remove compiled files:
```bash
make clean
```

## Credits

- Based on Chapter 8 of **"Programming in Haskell"** by **Graham Hutton**.
- Original implementation by **Pedro Vasconcelos** (2025).
- Created for the **PFL** (Programação Funcional e Lógica) course at **FEUP**.
