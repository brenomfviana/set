#!/bin/bash
clear
clear
echo "Compiling..."
ghc Parser.hs
rm *.hi *.o
