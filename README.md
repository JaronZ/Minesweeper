# Minesweeper

A minesweeper game made in Haskell using the console.  
This game is used as research in a school assignment.

## Setup

To play this game you will need the following programs,
which can be installed on the [haskell downloads page](https://www.haskell.org/downloads/):

- GHC
- Stack

After installing these you will also need the `System.Random` haskell module.
To install this module run the following command in the terminal:

```bash
stack ghci --package random
```

## Playing the game

To play the game run the following commands in the terminal:

```bash
cd src
stack ghci main.hs
```

Then once you are in ghci call the following function:

```haskell
main
```
