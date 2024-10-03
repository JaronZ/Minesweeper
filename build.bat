@echo off

if not exist ".\bin" mkdir bin

cd src
stack ghc -- main.hs -o ../bin/minesweeper
cd ..
