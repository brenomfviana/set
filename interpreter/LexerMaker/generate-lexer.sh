#!/bin/bash

echo "Converting to Haskell code..."
alex Lexer.x

echo "Compiling..."
ghc Lexer.hs
