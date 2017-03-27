-- Lexical Analyzer
-- Version: 25/03/2017
-- Author : Breno Viana
module LexicalAnalyzer (analyzer) where

-- Imports
import Data.List
import Data.List.Split

--
belongs:: String -> [String]
belongs str = split (onSublist ":in") str

--
difference:: [String] -> [String]
difference [] = []
difference (head:tail) = split (onSublist "!=") head ++ difference tail

--
biggerThan:: [String] -> [String]
biggerThan [] = []
biggerThan (head:tail) = split (onSublist "<=") head ++ biggerThan tail

--
lessThan:: [String] -> [String]
lessThan [] = []
lessThan (head:tail) = split (onSublist ">=") head ++ lessThan tail

--
equality:: [String] -> [String]
equality [] = []
equality (head:tail) =
    if head /= "!=" && head /= "<=" && head /= ">="
        then split (onSublist "=") head ++ equality tail
        else head : equality tail


-- Analyzes the code
---- Input : List with the words
---- Output: List with all lexemes of the program
analyzer:: [String] -> [String]
analyzer [] = []
analyzer (head:tail) = equality(lessThan(biggerThan(difference(belongs(head))))) ++ analyzer(tail)
