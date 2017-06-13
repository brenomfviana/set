-- Program State
-- Version: 120/06/2017
module State where

-- Internal imports
import Lexer
import Types

-- -----------------------------------------------------------------------------
-- State
-- -----------------------------------------------------------------------------

-- - State
-- Int         Controller counter
-- Scope       Current Scope
-- [Var]       Memory    (Variables)
-- [Statement] Statments (Functions, Procedures and UserTypes)
type State = (Scope, [Var], [Statement])

-- - Initializes the program state
-- Return Initial state
initState :: State
initState = ([], [], [])



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
insertVariable ((vid, vv), vs) (sc, [], st) = (sc, [((vid, vv), vs)], st)
insertVariable ((vid, vv), vs) (sc, m, st) =
    let nv = ((vid, vv), vs) in (sc, m ++ [nv], st)

-- - Update variable
-- Var    Variable
-- State  Current state
-- Return Updated state
updateVariable :: Var -> State -> State
updateVariable _ (_, [], _) = error "Error: Variable not found."
updateVariable ((Id id1 p1, v1), s1) (sc1, ((Id id2 p2, v2), s2) : m1, st1) =
    if id1 == id2 then (sc1, ((Id id1 p2, v1), s1) : m1, st1)
    else
        let (sc2, m2, st2) = updateVariable ((Id id1 p1, v1), s1) (sc1, m1, st1)
        in (sc1, ((Id id2 p2, v2), s2) : m2, st2)

-- - Remove variable
-- Var    Variable
-- State  Current state
-- Return Updated state
removeVariable :: Var -> State -> State
removeVariable _ (_, [], _) = error "Error: Variable not found."
removeVariable ((Id id1 p1, v1), s1) (sc1, ((Id id2 p2, v2), s2) : m1, st1) =
    if id1 == id2 then (sc1, m1, st1)
    else
        let (sc2, m2, st2) = removeVariable ((Id id1 p1, v1), s1) (sc1, m1, st1)
        in (sc2, ((Id id2 p2, v2), s2) : m2, st2)

-- - Get variable
-- Token  Variable ID
-- State  State
-- Return Variable
variableIsSet :: Token -> State -> Bool
variableIsSet _ (_, [], _) = False
variableIsSet (Id id1 p1) (sc, (((Id id2 p2), value), s2) : m, st) =
    if id1 == id2 then True
    else variableIsSet (Id id1 p1) (sc, m, st)

-- - Get variable
-- Token  Variable ID
-- State  State
-- Return Variable
getVariable :: Token -> State -> Var
getVariable _ (_, [], _) = error "Error: Variable not found."
getVariable (Id id1 p1) (sc, (((Id id2 p2), value), s2) : m, st) =
    if id1 == id2 then (((Id id2 p2), value), s2)
    else getVariable (Id id1 p1) (sc, m, st)

-- - Get type and value
-- Token  Variable ID
-- State  State
-- Return Variable
getVariableType :: Token -> State -> Token
getVariableType _ (_, [], _) = error "Error: Variable not found."
getVariableType (Id id1 p1) (sc, (((Id id2 p2), value), s2) : m, st) =
    if id1 == id2 then value
    else getVariableType (Id id1 p1) (sc, m, st)



-- -----------------------------------------------------------------------------
-- Scope
-- -----------------------------------------------------------------------------

type Scope = [String]

-- --------------------------------------
-- Scope handler
-- --------------------------------------

-- - Insert scope
-- String Current scope
-- State  State
-- Return Updated state
insertScope :: String -> State -> State
insertScope s  ([], m, st) = ([s], m, st)
insertScope s (sc, m, st) = (s:sc, m, st)

-- - Remove scope
-- String Current scope
-- State  State
-- Return Updated state
removeScope :: String -> State -> State
removeScope _  ([], _, _) = error "Error: The scope doesn't exits."
removeScope s1 (s2 : sc1, m1, st1) =
    if (s1 == s2) then (sc1, m1, st1)
    else let (sc2, m2, st2) = removeScope s1 (sc1, m1, st1)
         in (s2 : sc2, m2, st2)

-- - Get scope length
-- State Current state
-- Int   Scope length
getScopeLength :: State -> Int
getScopeLength (sc, _, _) = length(sc)

-- getScope ::



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
