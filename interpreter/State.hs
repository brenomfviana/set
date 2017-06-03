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

-- --------------------------------------------------------
-- Parser types
-- --------------------------------------------------------

--
type Parser      = ParsecT [Token] State Identity

--
type Interpreter = Parser  (Maybe (Type, Value))

--
type Statement   = Parser  (Maybe (Maybe (Type, Value)))

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
	deriving (Eq, Show)

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
	deriving (Eq, Show)

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

-- --------------------------------------
-- Variable handler
-- --------------------------------------

-- - Insert variable
-- VarDecl Variable declaration
-- State   Current state
-- Return  Updated state
insertVariable :: VarDecl -> State -> State
insertVariable (n, sc, t) (m, st) =
	let nv = (n, sc, t, getInitValue t) in (nv:m, st)

-- - Get variable
-- State  Current state
-- Key    Variable key
-- Return Updated state
getVariable :: State -> Key -> Var
getVariable ((vn, vsc, vt, vv):m, st) (n, sc) =
    if vn == n && vsc == sc then (vn, vsc, vt, vv)
    else getVariable (m, st) (n, sc)  -- Isso remove items da tabela?

-- - Get variable type
-- State  Current state
-- Key    Variable key
-- Return Variable type
getVariableType :: State -> Key -> Type
getVariableType s k = let (_, _, t, _) = getVar s k in t

-- - Update variable

-- -----------------------------------------------------------------------------
-- Statements
-- -----------------------------------------------------------------------------

-- --------------------------------------------------------
-- Procedure
-- --------------------------------------------------------

-- - Procedure declaration
-- String    Procedure name
-- Integer   Procedure scope ID
-- [VarDecl] Parameters
type ProcDec = (String, Integer, [VarDecl])

-- - Procedure
-- String    Procedure name
-- Integer   Procedure scope ID
-- [VarDecl] Parameters
-- [Token]   Statements
type Procedure = (String, Integer, [VarDecl], [Token])

-- --------------------------------------
-- Procedure handler
-- --------------------------------------

-- - Insert procedure
-- - Get procedure
-- - Update procedure

-- --------------------------------------------------------
-- Function
-- --------------------------------------------------------

-- - Function declaration
-- String    Function name
-- Integer   Function scope ID
-- [VarDecl] Parameters
-- Type      Return type
type FuncDec = (String, Integer, [VarDecl], Type)

-- - Function
-- String    Function name
-- Integer   Function scope ID
-- [VarDecl] Parameters
-- [Token]   Statements
-- Type      Return type
type Function = (String, Integer, [VarDecl], Type, [Token])

-- --------------------------------------
-- Function handler
-- --------------------------------------

-- - Insert function
-- - Get function
-- - Get function return type
-- - Update function

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

-- - Initializes the program state
initState :: State
initState = ([], [])

-- - Get init values
-- Type   Variable type
-- Return Initial variable value
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
