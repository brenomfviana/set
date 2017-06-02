#!/bin/bash

echo "Compiling..."
ghc Parser.hs
rm *.hi *.o
