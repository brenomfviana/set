-- Lexical Analyzer
-- Version: 25/03/2017
-- Author : Breno Viana
module LexicalAnalyzer (analyzer) where

-- Imports
import Data.List
import Data.List.Split

--
equality:: String -> [String]
equality str = split (onSublist "=") str

--
difference:: [String] -> [String]
difference [] = []
difference (head:tail) = split (onSublist "!=") head ++ difference tail

--
belongs:: [String] -> [String]
belongs [] = []
belongs (head:tail) = split (onSublist ":in") head ++ belongs tail

--
biggerThan:: [String] -> [String]
biggerThan [] = []
biggerThan (head:tail) = split (onSublist "<=") head ++ biggerThan tail

--
lessThan:: [String] -> [String]
lessThan [] = []
lessThan (head:tail) = split (onSublist ">=") head ++ lessThan tail


-- Analyzes the code
---- Input : List with the words
---- Output: List with all lexemes of the program
analyzer:: [String] -> [String]
analyzer [] = []
analyzer (head:tail) = lessThan(biggerThan(belongs(difference(equality(head))))) ++ analyzer(tail)
