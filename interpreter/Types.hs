-- Types
-- Version: 09/06/2017
module Types where

-- Imports
import Lexer

-- -----------------------------------------------------------------------------
-- Type checking
-- -----------------------------------------------------------------------------

-- - Get default value of different types
-- Type   Variable type
-- Return Initial variable value
getDefaultValue :: Token -> Token
getDefaultValue (Type "Nat"  pos) = Nat 0 pos
getDefaultValue (Type "Int"  pos) = Int 0 pos
getDefaultValue (Type "Real" pos) = Real 0.0 pos
getDefaultValue (Type "Bool" pos) = Bool False pos
-- getDefaultValue (Type "Univ" pos) = Univ "" pos
getDefaultValue (Type "Text" pos) = Text "" pos
-- getDefaultValue (Type "Pointer") = Pointer 0.0
-- getDefaultValue (Type "Set[" <type> "]") = "Set[" <type> "]" "\empty"

-- - Get type
-- [Token]          Token list
-- [(Token, Token)] State
-- Return           Variable type
getType :: Token -> [(Token, Token)] -> Token
getType _ [] = error "Variable not found."
getType (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                            else getType (Id id1 p1) t

-- - Get value
getValue :: Token -> String
getValue (Nat value _)  = show value
getValue (Int value _)  = show value
getValue (Real value _) = show value
getValue (Bool value _) = show value
getValue (Text value _) = show value
getValue _ = error "Error."