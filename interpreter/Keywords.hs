-- Types
-- Version: 13/06/2017
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
-- String Value
checkIfStmt :: Token -> Bool
checkIfStmt (If _) = True
checkIfStmt _ = False

-- - Get column if token
-- Token  If token
-- String Value
columnIfStmt :: Token -> Int
columnIfStmt (If (_, c)) = c
columnIfStmt _ = -1

-- - Check else statement
-- Token  Else token
-- String Value
checkElseStmt :: Token -> Bool
checkElseStmt (Else _) = True
checkElseStmt _ = False

-- - Get column else token
-- Token  Else token
-- String Value
columnElseStmt :: Token -> Int
columnElseStmt (Else (_, c)) = c
columnElseStmt _ = -1

-- - Check elseif statement
-- Token  ElseIf token
-- String Value
checkElseIfStmt :: Token -> Bool
checkElseIfStmt (Else_If _) = True
checkElseIfStmt _ = False

-- - Get column else token
-- Token  Else token
-- String Value
columnElseIfStmt :: Token -> Int
columnElseIfStmt (Else_If (_, c)) = c
columnElseIfStmt _ = -1

-- - Check endif statement
-- Token  End if token
-- String Value
checkEndIfStmt :: Token -> Bool
checkEndIfStmt (End_If _) = True
checkEndIfStmt _ = False

-- - Get column end token
-- Token  End if token
-- String Value
columnEndIfStmt :: Token -> Int
columnEndIfStmt (End_If (_, c)) = c
columnEndIfStmt _ = -1



-- --------------------------------------------------------
-- WHILE
-- --------------------------------------------------------

-- - Check while statement
-- Token  End token
-- String Value
checkWhileStmt :: Token -> Bool
checkWhileStmt (While _) = True
checkWhileStmt _ = False

-- - Get column while token
-- Token  While token
-- String Value
columnWhileStmt :: Token -> Int
columnWhileStmt (While (_, c)) = c
columnWhileStmt _ = -1

-- - Check endwhile statement
-- Token  End while token
-- String Value
checkEndWhileStmt :: Token -> Bool
checkEndWhileStmt (End_While _) = True
checkEndWhileStmt _ = False

-- - Get column end token
-- Token  End while token
-- String Value
columnEndWhileStmt :: Token -> Int
columnEndWhileStmt (End_While (_, c)) = c
columnEndWhileStmt _ = -1



-- --------------------------------------------------------
-- EXPRESSIONS
-- --------------------------------------------------------

-- - Check close parentheses
-- Token  Close parentheses token
-- String Value
checkCloseParentheses :: Token -> Bool
checkCloseParentheses (Close_Parentheses _) = True
checkCloseParentheses _ = False

-- - Check open bracket
-- Token  Open bracket token
-- String Value
checkOpenBracket :: Token -> Bool
checkOpenBracket (Open_Bracket _) = True
checkOpenBracket _ = False



-- --------------------------------------------------------
-- OTHERS STATEMENTS
-- --------------------------------------------------------

-- - Check end
-- Token  End token
-- String Value
checkEndStmt :: Token -> Bool
checkEndStmt (End _) = True
checkEndStmt _ = False
