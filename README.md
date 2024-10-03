# Minesweeper

A minesweeper game made in Haskell using the console.  
This game is used as research in a school assignment.

## Playing the game

**The game can only be played on windows. To compile it for your own operating system, see [Building](#building)**

To play the game, download an exe from the releases and then run it.

## Building

To build the game yourself run the following in your terminal of choice (Windows only):

### Command Prompt / PowerShell

```bash
.\build
```

## Developing

### Setup

To develop this program for yourself you will need the following programs,
which can be installed on the [haskell downloads page](https://www.haskell.org/downloads/):

- GHC
- Stack

After installing these you will also need the `System.Random` haskell module.
To install this module run the following command in the terminal:

```bash
stack ghci --package random
```

### Running the script

Then run the following commands in the terminal:

```bash
cd src
stack ghci main.hs
```

Then once you are in ghci call the following function:

```haskell
main
```
