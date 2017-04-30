#!/bin/bash

echo "Converting to Haskell code..."
alex LexicalAnalyzer.x

echo "Compiling..."
ghc LexicalAnalyzer.hs

echo "Tokenizing program..."
./LexicalAnalyzer < $1
