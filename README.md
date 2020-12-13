
# COMP4400 Connect 4
## Completed by Arshdeep Sidhu and Ferruccio Sisti

## Project Description
Final project for COMP4400 - Principles of Programming Languages. With a focus on functional programming practices and artificial intelligence. 
A player vs. AI version of the popular game Connect 4. AI uses a min/max algorithm to determine best possible move. 
Functional programming language of choice: Haskell

## Requirements

You will need to be able to compile and run Haskell (.hs) files. The following are our recommendations. Most commands can be substituted with your own package manager

### MacOS

``` brew install ghc ```
``` brew install cabal-install ```
``` cabal install gloss ```

### Linux

``` sudo apt-get install ghc ```
``` sudo apt-get install cabal-install ```
``` cabal install gloss ```

### Windows

Download WSL and refer to Linux commands.

## Compiling and Running the game

- Download or clone the rep
- Navigate to working directory
- Run the following command to compile the code
``` ghc Main.hs ```
- Run the following command to run the executable
``` ./Main ```
---
### Playing the game

If everything was installed and executed corrected, you should see the following window appear. 
![alt text](https://github.com/arshsidhu/COMP4400-Connect4/blob/main/Example.png "Example")

You will be using the number keys 1-7 (representing the columns) to place your tile.

Once there is a winner, no more moves will be allowed.

To quit the game, click on your terminal and type ```Ctrl+C```.