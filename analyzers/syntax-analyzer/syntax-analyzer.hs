-- Syntax Analyzer
-- Version: 25/03/2017
-- Author :

-- Imports
import Data.List

type Name = [Char]

type Address = Int

data Value =  Int     (Int)
            | String  ([Char])
            | Boolean (Bool)
            | Double  (Double)
            | Float   (Float)
            | Pointer (Address) deriving (Show)

type Escope = [Char]


type Symbol = (Name, Value, Escope)
type SymbolTable = [Symbol]


addSymbol :: Symbol -> SymbolTable -> SymbolTable
addSymbol (a, b, c) symbols = symbols ++ [(a, b, c)]
