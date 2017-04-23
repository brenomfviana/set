import Data.List

type Number = Integer
type Literal = [Char]

data Type = Int | String | Bool | Float | Char deriving (Show)
data Value = Number | Literal deriving (Show)
type Name = Literal


type Symbol = (Name, Type, Number, String)
type SymbolTable = [Symbol]


addSymbol :: (Name, Type, Number, String) -> [Symbol] -> [(Name, Type, Number, String)]
addSymbol (a, b, c, d) symbols = symbols ++ [(a, b, c, d)]
