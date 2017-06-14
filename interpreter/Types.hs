-- Types
-- Version: 13/06/2017
module Types where

-- External imports
import Data.List
-- Internal imports
import Lexer

-- -----------------------------------------------------------------------------
-- Values
-- -----------------------------------------------------------------------------

-- - Token null
tokenNull :: Token
tokenNull = (Id "_" (-1,-1))

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
getDefaultValue (Type "Array" pos) = Array ((Nat 0 pos),(Nat 0 pos),[]) pos
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
getValue (Array value _) = show (arrayToString value)
getValue (UserType value _) = show (usertypeToString value)
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
getDefaultArrayValue (Type "Array" p1) (Type "Nat"  p2) (Nat value p3) = Array ((Type "Nat"  p2), (Nat value p1), []) p1
getDefaultArrayValue (Type "Array" p1) (Type "Int"  p2) (Nat value p3) = Array ((Type "Int"  p2), (Nat value p1), []) p1
getDefaultArrayValue (Type "Array" p1) (Type "Real" p2) (Nat value p3) = Array ((Type "Real" p2), (Nat value p1), []) p1
getDefaultArrayValue (Type "Array" p1) (Type "Bool" p2) (Nat value p3) = Array ((Type "Bool" p2), (Nat value p1), []) p1
getDefaultArrayValue (Type "Array" p1) (Type "Text" p2) (Nat value p3) = Array ((Type "Text" p2), (Nat value p1), []) p1

-- - Get array size
-- Token  Array
-- Return Array size
getArraySize :: Token -> Token
getArraySize (Array (_, size, _) _) = size

-- - Get array value
-- Token  Array
-- Return Array value
getArrayValue :: Token -> [Token]
getArrayValue (Array (_, _, value) _) = value

-- - Get array type
-- Token  Array
-- Return Array type
getArrayType :: Token -> Token
getArrayType (Array (t, _, _) _) = t

-- - Get array item
-- Token  Array
-- Return Array item
getArrayItem :: Token -> Token -> Token
getArrayItem (Array (_, _, value) _) (Nat i _) = value!!i

-- - Set array item
-- Token  Array
-- Token  Position
-- Token  New value
-- Return Updated array
setArrayItem :: Token -> Token -> Token -> Token
setArrayItem (Array (t, s, value) p) (Nat i _) nv = (Array (t, s, (replaceNth i nv value) ) p)

-- - Convert a string in token list
-- Token    Array
-- Token    Array type
-- [String] Values
-- Return   Array values
toToken :: Token -> [String] -> [Token]
toToken _ [] = []
toToken (Array ((Type "Nat"  p2), s, v) p1) (sh:st) = (Nat  (stringToInt sh) p1) : toToken (Array ((Type "Nat" p2), s, v) p1) st
toToken (Array ((Type "Int"  p2), s, v) p1) (sh:st) = (Int  (stringToInt sh) p1) : toToken (Array ((Type "Int" p2), s, v) p1) st
toToken (Array ((Type "Real" p2), s, v) p1) (sh:st) = (Real (stringToFloat sh) p1) : toToken (Array ((Type "Real" p2), s, v) p1) st
toToken (Array ((Type "Bool" p2), s, v) p1) (sh:st) = (Bool (stringToBool sh) p1) : toToken (Array ((Type "Bool" p2), s, v) p1) st
toToken (Array ((Type "Text" p2), s, v) p1) (sh:st) = (Text sh p1) : toToken (Array ((Type "Text" p2), s, v) p1) st

-- - Convert an array in a string
-- Token  Array
-- Return Array value
arrayToString :: (Token, Token, [Token]) -> [String]
arrayToString (_, _, []) = []
arrayToString ((Type "Nat"  p2), (Nat v1 p1), (Nat  v3 p3):t) = [(show v3)] ++ arrayToString ((Type "Nat"  p2), (Nat v1 p1), t)
arrayToString ((Type "Int"  p2), (Nat v1 p1), (Int  v3 p3):t) = [(show v3)] ++ arrayToString ((Type "Int"  p2), (Nat v1 p1), t)
arrayToString ((Type "Real" p2), (Nat v1 p1), (Real v3 p3):t) = [(show v3)] ++ arrayToString ((Type "Real" p2), (Nat v1 p1), t)
arrayToString ((Type "Bool" p2), (Nat v1 p1), (Bool v3 p3):t) = [(show v3)] ++ arrayToString ((Type "Bool" p2), (Nat v1 p1), t)
arrayToString ((Type "Text" p2), (Nat v1 p1), (Text v3 p3):t) = [(show v3)] ++ arrayToString ((Type "Text" p2), (Nat v1 p1), t)

-- - Replate nth item
-- Int Index Position
-- Token     New value
-- [Token]   Array
-- Return    Updated array
replaceNth :: Int -> Token -> [Token] -> [Token]
replaceNth i nv (x:xs)
     | i == 0 = nv:xs
     | otherwise = x:replaceNth (i-1) nv xs



 -- --------------------------------------------------------
 -- Matrix
 -- --------------------------------------------------------

 -- - Get default matrix value
 -- Token  Matrix
 -- Token  Matrix lines
 -- Token  Matrix columns
 -- Return Initial matrix value
 -- getDefaultMatrixValue :: Token -> Token -> Token -> Token
 -- getDefaultMatrixValue (Type "Matrix" p1) (Type "Nat"  p2) (Nat v1 p3) (Nat v2 p4) = Matrix ((Type "Nat"  p2), (Nat v1 p3), (Nat v2 p4), [[]]) p1
 -- getDefaultMatrixValue (Type "Matrix" p1) (Type "Int"  p2) (Nat v1 p3) (Nat v2 p4) = Matrix ((Type "Int"  p2), (Nat v1 p3), (Nat v2 p4), [[]]) p1
 -- getDefaultMatrixValue (Type "Matrix" p1) (Type "Real" p2) (Nat v1 p3) (Nat v2 p4) = Matrix ((Type "Real" p2), (Nat v1 p3), (Nat v2 p4), [[]]) p1
 -- getDefaultMatrixValue (Type "Matrix" p1) (Type "Bool" p2) (Nat v1 p3) (Nat v2 p4) = Matrix ((Type "Bool" p2), (Nat v1 p3), (Nat v2 p4), [[]]) p1
 -- getDefaultMatrixValue (Type "Matrix" p1) (Type "Text" p2) (Nat v1 p3) (Nat v2 p4) = Matrix ((Type "Text" p2), (Nat v1 p3), (Nat v2 p4), [[]]) p1



-- --------------------------------------------------------
-- Typedef
-- --------------------------------------------------------

-- - Get typedef fields
-- [Token] Typedef body
-- Return  Fields
getFields :: [Token] -> [Token]
getFields [] = []
getFields ((Id n p):t) = (Id n p) : getFields t
getFields ((Type "Nat"  p):t) = (Type "Nat"  p) : getFields t
getFields ((Type "Int"  p):t) = (Type "Int"  p) : getFields t
getFields ((Type "Real" p):t) = (Type "Real" p) : getFields t
getFields ((Type "Bool" p):t) = (Type "Bool" p) : getFields t
getFields ((Type "Text" p):t) = (Type "Text" p) : getFields t
getFields (x:t) = getFields t

-- - Get default user type value
-- Token   User type
-- [Token] Fields
-- Token   Get default value of each type
getDefaultUserTypeValue :: Token -> [Token] -> Token
getDefaultUserTypeValue (Id td p1) fs = (UserType ((Id td p1), (getDefaultFieldValues fs)) p1)

-- - Get defaut values of each field
-- [Token] Fields
-- Return  Values
getDefaultFieldValues :: [Token] -> [(Token, Token)]
getDefaultFieldValues [] = []
getDefaultFieldValues (v:f:t) = (f, (getDefaultValue v)) : getDefaultFieldValues t

-- - Convert a user type in a string
usertypeToString :: (Token, [(Token, Token)]) -> [String]
usertypeToString (n, fs) = fieldToString fs

-- - Convert each field to string
fieldToString :: [(Token, Token)] -> [String]
fieldToString [] = []
fieldToString ((v,f):t) = (getValue f) : fieldToString t


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
cast (Nat  _ _)  (Int i p) = if i < 0 then Int i p
                             else Nat i p
cast (Int  _ _)  (Nat i p) = Int i p
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
