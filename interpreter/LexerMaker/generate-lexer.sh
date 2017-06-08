# mv "Lexer.hs" "/home/ubuntu/workspace/interpreter/Lexer.hs"#!/bin/bash

echo "Converting to Haskell code..."
alex Lexer.x

echo "Moving..."
mv "Lexer.hs" $(cd ../ && pwd)
