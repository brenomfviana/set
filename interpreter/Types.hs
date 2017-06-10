-- Types
-- Version: 09/06/2017
module Types where

-- Imports
import Lexer

-- -----------------------------------------------------------------------------
-- Values
-- -----------------------------------------------------------------------------

-- - Get default value of different types
-- Type   Variable type
-- Return Initial variable value
getDefaultValue :: Token -> Token
getDefaultValue (Type "Nat"  pos) = Nat 0 pos
getDefaultValue (Type "Int"  pos) = Int 0 pos
getDefaultValue (Type "Real" pos) = Real 0.0 pos
getDefaultValue (Type "Bool" pos) = Bool False pos
getDefaultValue (Type "Text" pos) = Text "" pos
-- getDefaultValue (Type "Pointer") = Pointer 0.0

-- - Get value
getValue :: Token -> String
getValue (Nat  value _) = show value
getValue (Int  value _) = show value
getValue (Real value _) = show value
getValue (Bool value _) = show value
getValue (Text value _) = show value
getValue _ = error "Error."

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
cast (Nat  _ _)  (Nat i p) = if i < 0 then error "Invalid assignment."
                             else Nat i p
cast (Int  _ _)  (Nat i p) = if i < 0 then Int i p
                             else Nat i p
cast (Int  _ _)  (Int i p) = Int i p
cast (Real _ _)  (Nat i p) = let x = integerToFloat(i) in Real x p
cast (Real _ _)  (Int i p) = let x = integerToFloat(i) in Real x p
cast (Real _ _) (Real i p) = Real i p
cast (Bool _ _) (Bool i p) = Bool i p
cast (Text _ _) (Text i p) = Text i p
cast _ _ = error "Invalid cast."

-- -----------------------------------------------------------------------------
-- Other necessary functions
-- -----------------------------------------------------------------------------

-- - Convert int in float
-- Int    Integer value
-- Return Float value
integerToFloat :: Int -> Float
integerToFloat i = do
    let r = (fromIntegral(toInteger i) / fromIntegral 1) in r