-- Types
-- Version: 11/06/2017
module Keywords where

-- Internal imports
import Lexer

-- -----------------------------------------------------------------------------
-- Keywords checking
-- -----------------------------------------------------------------------------

-- - Check if statement
-- Token  If token
-- String Value
checkIfStmt :: Token -> String
checkIfStmt (If _) = show True
checkIfStmt _ = show False

-- - Get column if token
-- Token  If token
-- String Value
columnIfStmt :: Token -> Int
columnIfStmt (If (_, c)) = c
columnIfStmt _ = -1

-- - Check else statement
-- Token  Else token
-- String Value
checkElseStmt :: Token -> String
checkElseStmt (Else _) = show True
checkElseStmt _ = show False

-- - Get column else token
-- Token  Else token
-- String Value
columnElseIfStmt :: Token -> Int
columnElseIfStmt (Else_If (_, c)) = c
columnElseIfStmt _ = -1

-- - Check elseif statement
-- Token  ElseIf token
-- String Value
checkElseIfStmt :: Token -> String
checkElseIfStmt (Else_If _) = show True
checkElseIfStmt _ = show False

-- - Get column else token
-- Token  Else token
-- String Value
columnElseStmt :: Token -> Int
columnElseStmt (Else (_, c)) = c
columnElseStmt _ = -1

-- - Check endif statement
-- Token  End token
-- String Value
checkEndIfStmt :: Token -> String
checkEndIfStmt (End_If _) = show True
checkEndIfStmt _ = show False

-- - Get column end token
-- Token  End token
-- String Value
columnEndIfStmt :: Token -> Int
columnEndIfStmt (End_If (_, c)) = c
columnEndIfStmt _ = -1

-- - Get column end token
-- Token  End token
-- String Value
columnEndWhileStmt :: Token -> Int
columnEndWhileStmt (End_While (_, c)) = c
columnEndWhileStmt _ = -1

-- - Check endwhile statement
-- Token  End token
-- String Value
checkEndWhileStmt :: Token -> String
checkEndWhileStmt (End_While _) = show True
checkEndWhileStmt _ = show False

-- - Get column while token
-- Token  While token
-- String Value
columnWhileStmt :: Token -> Int
columnWhileStmt (While (_, c)) = c
columnWhileStmt _ = -1

-- - Check end
-- Token  End token
-- String Value
checkEndStmt :: Token -> String
checkEndStmt (End _) = show True
checkEndStmt _ = show False
