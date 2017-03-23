-- Lexical Analyzer
-- Version: 23/03/2017
-- Author : Breno Viana
module LexicalAnalyzer (analyzer) where

	-- Imports
	import Data.List.Split
	import Data.List

	-- 
	token:: [String] -> [String]
	token [] = []

	-- Analyzes the code
	---- Input : List with the words
	---- Output: List with all lexemes of the program
	analyzer:: [String] -> [String]
	analyzer [] = []
	analyzer (head,tail) = token(head) ++ analyzer(tail)