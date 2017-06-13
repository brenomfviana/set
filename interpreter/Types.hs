-- Types
-- Version: 11/06/2017
module Types where

-- Internal imports
import Lexer

-- -----------------------------------------------------------------------------
-- Values
-- -----------------------------------------------------------------------------

-- - Get default value of different types
-- Type   Variable type
-- Return Initial variable value
getTokenName :: Token -> String
getTokenName (Id s _) = s
getTokenName _ = error "Invalid token format."

-- - Get default value of different types
-- Type   Variable type
-- Return Initial variable value
getTokenPosition :: Token -> String
getTokenPosition (Id _ pos) = show pos
getTokenPosition _ = error "Invalid token format."

-- - Get default value of different types
-- Token  Variable type
-- Return Initial variable value
getDefaultValue :: Token -> Token
getDefaultValue (Type "Nat"   pos) = Nat 0 pos
getDefaultValue (Type "Int"   pos) = Int 0 pos
getDefaultValue (Type "Real"  pos) = Real 0.0 pos
getDefaultValue (Type "Bool"  pos) = Bool False pos
getDefaultValue (Type "Text"  pos) = Text "" pos
getDefaultValue (Type "Array" pos) = Array [] pos
-- getDefaultValue (Type "Pointer") = Pointer 0.0

-- - Get value
-- Token  Literal token
-- String Value
getValue :: Token -> String
getValue (Nat   value _) = show value
getValue (Int   value _) = show value
getValue (Real  value _) = show value
getValue (Bool  value _) = show value
getValue (Text  value _) = removeQuote(show value)
getValue (Array value _) = show (toString value)
getValue _ = error "Error: Value not found."

-- - Get ID name
-- Token  ID token
-- String Name
getIdName :: Token -> String
getIdName (Id name _) = name
getIdName _ = error "Error: Name not found."



-- --------------------------------------------------------
-- Array
-- --------------------------------------------------------

-- - Get default array value
-- Token  Array
-- Token  Array size
-- Return Initial array value
getDefaultArrayValue :: Token -> Token -> Token -> Token
getDefaultArrayValue (Type "Array" p1) (Type "Nat"  p2) (Nat value p3) = Array ((Nat value p1):[(Type "Nat"  p2)]) p1
getDefaultArrayValue (Type "Array" p1) (Type "Int"  p2) (Nat value p3) = Array ((Nat value p1):[(Type "Int"  p2)]) p1
getDefaultArrayValue (Type "Array" p1) (Type "Real" p2) (Nat value p3) = Array ((Nat value p1):[(Type "Real" p2)]) p1
getDefaultArrayValue (Type "Array" p1) (Type "Bool" p2) (Nat value p3) = Array ((Nat value p1):[(Type "Bool" p2)]) p1
getDefaultArrayValue (Type "Array" p1) (Type "Text" p2) (Nat value p3) = Array ((Nat value p1):[(Type "Text" p2)]) p1

-- - Get array value
-- Token  Array
-- Return Array value
getArrayValue :: Token -> [Token]
getArrayValue (Array value _) = value

-- - Convert a string in token list
-- Token    Array
-- Token    Array type
-- [String] Values
-- Return   Array values
toToken :: Token -> [String] -> [Token]
toToken _ [] = []
toToken (Array (h:(Type "Nat"  p2):v) p1) (sh:st) = (Nat  (stringToInt sh) p1) : toToken (Array (h:(Type "Nat" p2):v) p1) st
toToken (Array (h:(Type "Int"  p2):v) p1) (sh:st) = (Int  (stringToInt sh) p1) : toToken (Array (h:(Type "Int" p2):v) p1) st
toToken (Array (h:(Type "Real" p2):v) p1) (sh:st) = (Real (stringToFloat sh) p1) : toToken (Array (h:(Type "Real" p2):v) p1) st
toToken (Array (h:(Type "Bool" p2):v) p1) (sh:st) = (Bool (stringToBool sh) p1) : toToken (Array (h:(Type "Bool" p2):v) p1) st
toToken (Array (h:(Type "Text" p2):v) p1) (sh:st) = (Text sh p1) : toToken (Array (h:(Type "Text" p2):v) p1) st

-- - Convert an array in a string
-- Token  Array
-- Return Array value
toString :: [Token] -> [String]
toString [] = []
toString ((Nat _ _):(Type _ _):t) = toString t
toString ((Nat  value _):t) = [(show value)] ++ toString t
toString ((Int  value _):t) = [(show value)] ++ toString t
toString ((Real value _):t) = [(show value)] ++ toString t
toString ((Bool value _):t) = [(show value)] ++ toString t
toString ((Text value _):t) = [(show value)] ++ toString t



-- -----------------------------------------------------------------------------
-- Type checking
-- -----------------------------------------------------------------------------

-- - Check whether types are compatible
-- ParsecT              ParsecT
-- [Token]              Token list
-- ([Var], [Statement]) State
compatible :: Token -> Token -> Bool
compatible (Nat  _ _)  (Nat _ _) = True
compatible (Int  _ _)  (Int _ _) = True
compatible (Int  _ _)  (Nat _ _) = True
compatible (Real _ _) (Real _ _) = True
compatible (Real _ _)  (Int _ _) = True
compatible (Real _ _)  (Nat _ _) = True
compatible (Bool _ _) (Bool _ _) = True
compatible (Text _ _) (Text _ _) = True
-- compatible (Pointer _ _) (Pointer _ _) = True
compatible _ _ = False

-- - Cast
-- Token  Variable type
-- Token  Expression type
-- Return New expression type
cast :: Token -> Token -> Token
cast (Nat  _ _)  (Nat i p) = if i < 0 then error "Error: Invalid assignment."
                             else Nat i p
cast (Int  _ _)  (Nat i p) = if i < 0 then Int i p
                             else Nat i p
cast (Int  _ _)  (Int i p) = Int i p
cast (Real _ _)  (Nat i p) = let x = integerToFloat(i) in Real x p
cast (Real _ _)  (Int i p) = let x = integerToFloat(i) in Real x p
cast (Real _ _) (Real i p) = Real i p
cast (Bool _ _) (Bool i p) = Bool i p
cast (Text _ _) (Text i p) = Text i p
cast _ _ = error "Error: Invalid cast."

-- - Cast
-- Token  Variable type
-- Token  Expression type
-- Return New expression type
inputCast :: Token -> Token -> Token
inputCast (Text _ _) (Text i p) =
    Text (removeQuote(i)) p
inputCast (Nat  _ _) (Text i p) = let x = stringToInt(removeQuote(i))
    in Nat  x p
inputCast (Int  _ _) (Text i p) = let x = stringToInt(removeQuote(i))
    in Int  x p
inputCast (Real _ _) (Text i p) = let x = stringToFloat(removeQuote(i))
    in Real x p
inputCast (Bool _ _) (Text i p) = let x = stringToBool(removeQuote(i))
    in Bool x p
inputCast _ _ = error "Error: Invalid cast."



-- -----------------------------------------------------------------------------
-- Other necessary functions
-- -----------------------------------------------------------------------------

-- - Convert int to float
-- Int    Integer value
-- Return Float value
integerToFloat :: Int -> Float
integerToFloat i = do
    let r = (fromIntegral(toInteger i) / fromIntegral 1) in r

-- - Convert string to int
-- Int    String value
-- Return Integer value
stringToInt :: String -> Int
stringToInt i = read i :: Int

-- - Convert string to float
-- Int    String value
-- Return Float value
stringToFloat :: String -> Float
stringToFloat i = read i :: Float

-- - Convert string to bool
-- Int    String value
-- Return Bool value
stringToBool :: String -> Bool
stringToBool i = read i :: Bool

-- - Remove quotes
-- String String
-- String String
removeQuote :: String -> String
removeQuote s = let s1:a = s
                    s2:b = reverse a
                    y = reverse b
                    in y
