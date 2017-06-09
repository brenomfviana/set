#!/bin/bash
clear
clear
echo "Compiling..."
ghc set.hs
rm *.hi *.o
