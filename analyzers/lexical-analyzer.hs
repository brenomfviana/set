-- Lexical Analyzer
-- Version: 25/03/2017
-- Author : Breno Viana
module LexicalAnalyzer (analyzer) where

-- Imports
import Data.List
import Data.List.Split

--
token:: String -> [String]
token str = split (onSublist "=") str

-- Analyzes the code
---- Input : List with the words
---- Output: List with all lexemes of the program
analyzer:: [String] -> [String]
analyzer [] = []
analyzer (head:tail) = token(head) ++ analyzer(tail)
