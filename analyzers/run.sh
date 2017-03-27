#!/bin/bash

echo "Converting to Haskell code ..."
alex tokens.x

echo "Compiling ..."
ghc tokens.hs

echo "Tokenizing program ..."
./tokens < $1