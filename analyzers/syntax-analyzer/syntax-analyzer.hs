-- Syntax Analyzer
-- Version: 30/04/2017
-- Author : Felipe Barbalho

-- Imports
import Data.List

-- Variable name
type Name = [Char]
-- Variable address
type Address = Int
-- Variable value
data Value =  Int     (Int)
            | String  ([Char])
            | Boolean (Bool)
            | Double  (Double)
            | Float   (Float)
            | Pointer (Address) deriving (Show)
-- Scope of the variable
type Escope = [Char]
-- Symbol
type Symbol = (Name, Value, Escope)
-- Symbol table
type SymbolTable = [Symbol]

-- Add a symbol to symbol table
addSymbol :: Symbol -> SymbolTable -> SymbolTable
addSymbol (a, b, c) symbols = symbols ++ [(a, b, c)]

--
