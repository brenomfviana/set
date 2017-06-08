# mv "Lexer.hs" "/home/ubuntu/workspace/interpreter/Lexer.hs"#!/bin/bash

echo "Converting to Haskell code..."
alex Lexer.x

echo "Moving..."
# Get dir name
# DIRNAME=$(dirname "$(realpath 0)")
# N=""
# mv "Lexer.hs" "${DIRNAME/LexerMaker/$N}Lexer.hs"
mv "Lexer.hs" "/home/ubuntu/workspace/interpreter/Lexer.hs"
