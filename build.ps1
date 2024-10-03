New-Item -ItemType Directory -Force -Path ./bin
cd src
stack ghc -- main.hs -o ../bin/minesweeper
cd ..
