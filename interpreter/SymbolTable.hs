-- Symbol Table
-- Version: 30/04/2017
-- Author : Felipe Barbalho
module SymbolTable where

-- Import
import Parser
import Data.List
import Data.Tuple

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


-- Remove a symbol to symbol table
removeSymbol :: SymbolTable -> Name -> Escope -> SymbolTable
-- Error symbol not found
removeSymbol [] _ _ = []
-- Search and remove symbol
removeSymbol (head:tail) name escope = let (n, _, e) = head in
                                        if (name == n && escope == e) then removeSymbol tail
                                        else head : findSymbol tail name escope


-- Update a symbol to symbol table
updateSymbol :: SymbolTable -> Name -> Escope -> Value -> SymbolTable
-- Error symbol not found
updateSymbol [] _ _ _ = []
-- Search and update symbol
updateSymbol (head:tail) name escope value = let (n, _, e) = head in
                                        if (name == n && escope == e) then setValue head value : tail
                                        else head : findSymbol tail name escope


-- Set value
setValue :: Symbol -> Value -> Symbol
setValue (n, v, e) value = (n, value, e)



-- Find symbol
findSymbol :: SymbolTable -> Name -> Escope -> Symbol
-- Error symbol not found
findSymbol [] _ _ = ("Not Found", "NF", "NULL")
-- Search symbol
findSymbol (head:tail) name escope = let (n, _, e) = head in
                                        if (name == n && escope == e) then head
                                        else head : findSymbol tail name escope
