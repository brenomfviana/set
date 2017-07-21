-- Types
-- Version: 14/06/2017
module Keywords where

-- Internal imports
import Lexer

-- -----------------------------------------------------------------------------
-- Keywords checking
-- -----------------------------------------------------------------------------

-- --------------------------------------------------------
-- TYPES
-- --------------------------------------------------------

-- - Check natural type statement
checkNatType :: Token -> Bool
checkNatType (Nat _ _) = True
checkNatType _ = False

-- - Check array type statement
checkArrayType :: Token -> Bool
checkArrayType (Array _ _) = True
checkArrayType _ = False



-- --------------------------------------------------------
-- IF
-- --------------------------------------------------------

-- - Check if statement
-- Token  If token
-- Return True if is the token, false otherwise
checkIfStmt :: Token -> Bool
checkIfStmt (If _) = True
checkIfStmt _ = False

-- - Get column if token
-- Token  If token
-- Return True if is the token, false otherwise
columnIfStmt :: Token -> Int
columnIfStmt (If (_, c)) = c
columnIfStmt _ = -1

-- - Check else statement
-- Token  Else token
-- Return True if is the token, false otherwise
checkElseStmt :: Token -> Bool
checkElseStmt (Else _) = True
checkElseStmt _ = False

-- - Get column else token
-- Token  Else token
-- Return True if is the token, false otherwise
columnElseStmt :: Token -> Int
columnElseStmt (Else (_, c)) = c
columnElseStmt _ = -1

-- - Check elseif statement
-- Token  ElseIf token
-- Return True if is the token, false otherwise
checkElseIfStmt :: Token -> Bool
checkElseIfStmt (Else_If _) = True
checkElseIfStmt _ = False

-- - Get column else token
-- Token  Else token
-- Return True if is the token, false otherwise
columnElseIfStmt :: Token -> Int
columnElseIfStmt (Else_If (_, c)) = c
columnElseIfStmt _ = -1

-- - Check endif statement
-- Token  End if token
-- Return True if is the token, false otherwise
checkEndIfStmt :: Token -> Bool
checkEndIfStmt (End_If _) = True
checkEndIfStmt _ = False

-- - Get column end token
-- Token  End if token
-- Return True if is the token, false otherwise
columnEndIfStmt :: Token -> Int
columnEndIfStmt (End_If (_, c)) = c
columnEndIfStmt _ = -1



-- --------------------------------------------------------
-- WHILE
-- --------------------------------------------------------

-- - Check while statement
-- Token  End token
-- Return True if is the token, false otherwise
checkWhileStmt :: Token -> Bool
checkWhileStmt (While _) = True
checkWhileStmt _ = False

-- - Get column while token
-- Token  While token
-- Return True if is the token, false otherwise
columnWhileStmt :: Token -> Int
columnWhileStmt (While (_, c)) = c
columnWhileStmt _ = -1

-- - Check endwhile statement
-- Token  End while token
-- Return True if is the token, false otherwise
checkEndWhileStmt :: Token -> Bool
checkEndWhileStmt (End_While _) = True
checkEndWhileStmt _ = False

-- - Get column end token
-- Token  End while token
-- Return True if is the token, false otherwise
columnEndWhileStmt :: Token -> Int
columnEndWhileStmt (End_While (_, c)) = c
columnEndWhileStmt _ = -1



-- --------------------------------------------------------
-- EXPRESSIONS
-- --------------------------------------------------------

-- - Check close parentheses
-- Token  Close parentheses token
-- Return True if is the token, false otherwise
checkCloseParentheses :: Token -> Bool
checkCloseParentheses (Close_Parentheses _) = True
checkCloseParentheses _ = False

-- - Check open bracket
-- Token  Open bracket token
-- Return True if is the token, false otherwise
checkOpenBracket :: Token -> Bool
checkOpenBracket (Open_Bracket _) = True
checkOpenBracket _ = False



-- --------------------------------------------------------
-- OTHERS STATEMENTS
-- --------------------------------------------------------

-- - Check end
-- Token  End token
-- Return True if is the token, false otherwise
checkEndStmt :: Token -> Bool
checkEndStmt (End _) = True
checkEndStmt _ = False
