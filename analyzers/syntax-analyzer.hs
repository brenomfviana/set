
type Number = Integer
type Literal = [Char]

data Type = Int | String | Bool | Float | Char deriving (Show)
data Value = Number | Literal deriving (Show)
type Name = Literal

-- symbols::[(Name, Type, Value, String)]
-- Não consegui usar um Value Genérico
symbols::[(Name, Type, Number, String)] 
symbols = [("cont", Int, 10, "if_01")] 

-- Show Comand: print symbols