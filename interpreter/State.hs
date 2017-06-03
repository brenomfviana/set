-- Program State
-- Version: 03/06/2017
module State where

-- Imports
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Lexer

-- -----------------------------------------------------------------------------
-- State
-- -----------------------------------------------------------------------------

-- - State
-- [Var]       Memory    (Variables)
-- [Statement] Statments (Functions, Procedures and UserTypes)
type State = ([Var], [Statement])

-- - Search key
-- String  Type name
-- Integer Type scope ID
type Key = (String, Integer)

-- -----------------------------------------------------------------------------
-- Memory
-- -----------------------------------------------------------------------------

-- --------------------------------------------------------
-- Types
-- --------------------------------------------------------

-- Types
data Type =
      AtomicType  String
  	| ArrayType   Integer Type
	| PointerType Type
	| ProcType    ProcDecl
	| FuncType    FuncDecl
	deriving (Eq,Show)

-- Values
data Value =
      NatValue     Int
    | IntValue     Int
    | RealValue    Double
  	| BoolValue    Bool
  	| TextValue    String
    | ArrayValue   [Value]
	| PointerValue Key
	| ProcValue    Key
	| FuncValue    Key
    | UserValue    [Value]
	deriving (Eq,Show)

-- --------------------------------------------------------
-- Variables
-- --------------------------------------------------------


-- - Variable declaration
-- String  Variable name
-- Integer Variable scope ID
-- Type    Variable type
type VarDecl = (String, Integer, Type)

-- - Variable
-- String  Variable name
-- Integer Variable scope ID
-- Type    Variable type
-- Value   Variable value
type Var = (String, Integer, Type, Value)

-- - Updated variable
-- Var  Variable
-- Bool --
type UpdatedVar = (Var, Bool)

-- -----------------------------------------------------------------------------
-- Statements
-- -----------------------------------------------------------------------------

-- --------------------------------------------------------
-- Procedure
-- --------------------------------------------------------

-- --------------------------------------
-- Procedure handler
-- --------------------------------------

-- --------------------------------------------------------
-- Function
-- --------------------------------------------------------

-- --------------------------------------
-- Function handler
-- --------------------------------------

-- --------------------------------------------------------
-- User types
-- --------------------------------------------------------

-- - Field
-- String Variable name
-- Type   Variable type
type Field = (String, Type)

-- - User type
-- String  Type name
-- [Field] Fields
type UserType = (String, [Field])

-- -----------------------------------------------------------------------------
-- Init values
-- -----------------------------------------------------------------------------

-- Initializes the program state
initState :: State
initState = ([], [])

-- Get init values
getInitValue :: Type -> Value
getInitValue (AtomicType  "Nat") = NatValue  0
getInitValue (AtomicType  "Int") = IntValue  0
getInitValue (AtomicType "Real") = RealValue 0.0
getInitValue (AtomicType "Bool") = BoolValue False
getInitValue (AtomicType "Text") = TextValue ""
-- getInitValue (AtomicType _) = UserValue []
-- getInitValue (PointerType _) = PointerValue ("", 0)
-- getInitValue (ArrayType _ n) = ArrayValue []
-- getInitValue (FuncType _) = FuncValue ("", -1)
-- getInitValue (ProcType _) = ProcValue ("", -1)
