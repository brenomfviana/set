-- Program State
-- Version: 03/06/2017
module State where

-- Imports
import Lexer
import Types

-- -----------------------------------------------------------------------------
-- State
-- -----------------------------------------------------------------------------

-- - State
-- [Var]       Memory    (Variables)
-- [Statement] Statments (Functions, Procedures and UserTypes)
type State = ([Var], [Statement])



-- -----------------------------------------------------------------------------
-- Memory
-- -----------------------------------------------------------------------------

-- --------------------------------------------------------
-- Variables
-- --------------------------------------------------------

-- - Variable
-- (Token, Token) Variable and it's value
-- String         Variable scope ID
type Var = ((Token, Token), String)

-- --------------------------------------
-- Variable handler
-- --------------------------------------

-- - Insert variable
-- Var    Variable
-- State  Current state
-- Return Updated state
insertVariable :: Var -> State -> State
insertVariable ((id, v), s) ([], st) = ([((id, v), s)], st)
insertVariable ((id, v), s) (m, st) = let nv = ((id, v), s) in (m ++ [nv], st)

-- - Update variable
-- Var    Variable
-- State  Current state
-- Return Updated state
updateVariable :: Var -> State -> State
updateVariable _ ([], _) = error "Variable not found."
updateVariable ((Id id1 p1, v1), s1) (((Id id2 p2, v2), s2) : m1, st1) =
    if id1 == id2 then (((Id id1 p2, v1), s1) : m1, st1)
    else let (m2, st2) = updateVariable ((Id id1 p1, v1), s1) (m1, st1)
         in (((Id id2 p2, v2), s2) : m2, st2)

-- - Remove variable
-- Var    Variable
-- State  Current state
-- Return Updated state
removeVariable :: Var -> State -> State
removeVariable _ ([], _) = error "Variable not found."
removeVariable ((Id id1 p1, v1), s1) (((Id id2 p2, v2), s2) : m1, st1) =
    if id1 == id2 then (m1, st1)
    else let (m2, st2) = removeVariable ((Id id1 p1, v1), s1) (m1, st1)
         in (((Id id2 p2, v2), s2) : m2, st2)

-- - Get variable
-- Token  Variable ID
-- State  State
-- Return Variable
getVariable :: Token -> State -> Var
getVariable _ ([], _) = error "Variable not found."
getVariable (Id id1 p1) ((((Id id2 p2), value), s2) : m, st) =
    if id1 == id2 then (((Id id2 p2), value), s2)
    else getVariable (Id id1 p1) (m, st)

-- - Get type and value
-- Token  Variable ID
-- State  State
-- Return Variable
getVariableType :: Token -> State -> Token
getVariableType _ ([], _) = error "Variable not found."
getVariableType (Id id1 p1) ((((Id id2 p2), value), s2) : m, st) =
    if id1 == id2 then value
    else getVariableType (Id id1 p1) (m, st)

-- getVariableValue :: Token -> State -> Var

-- - Update variable

-- -----------------------------------------------------------------------------
-- Statements
-- -----------------------------------------------------------------------------

type Statement = Var

-- --------------------------------------------------------
-- Procedure
-- --------------------------------------------------------

{-
-- - Procedure declaration
-- String    Procedure name
-- Integer   Procedure scope ID
-- [VarDecl] Parameters
type ProcDecl = (String, Integer, [VarDecl])

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
-}

-- -----------------------------------------------------------------------------
-- Init values
-- -----------------------------------------------------------------------------

-- - Initializes the program state
initState :: State
initState = ([], [])
