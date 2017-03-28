#!/bin/bash

echo "Converting to Haskell code..."
alex lexical-analyzer.x

echo "Compiling..."
ghc lexical-analyzer.hs

echo "Tokenizing program..."
./lexical-analyzer < $1
