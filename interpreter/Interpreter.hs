-- Parser (Syntax Analyzer)
-- Version: 09/06/2017
module Interpreter where

-- External imports
import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Parsec
-- Internal imports
import Lexer
import Types
import Parser
import State
import Expressions

-- -----------------------------------------------------------------------------
-- Parser to nonterminals
-- -----------------------------------------------------------------------------

-- - Program
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
program :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
program = do
    a <- programToken
    b <- idToken
    updateState(insertScope(getIdName b))
    c <- stmts
    d <- endToken
    updateState(removeScope(getIdName b))
    e <- idToken
    eof
    return (a:[b] ++ c ++ [e])

-- - Variable declarations
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
varDecls :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
varDecls = do
    first <- varDecl
    next  <- remainingVarDecls
    return (first ++ next)

-- - Variable declaration
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
varDecl :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
varDecl = do
    a <- typeToken
    b <- colonToken
    c <- idToken
    d <- semiColonToken
    updateState(insertVariable((c, getDefaultValue(a)), "m"))
    return (a:b:[c])

-- - Variable declaration remaining
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
remainingVarDecls :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
remainingVarDecls = (do a <- varDecls
                        return (a)) <|> (return [])

-- - Statements
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
stmts :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
stmts = do
    first <- assign <|> varDecls <|> printf <|> ifStmt
    next  <- remainingStmts
    return (first ++ next)

-- - Statements remaining
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
remainingStmts :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
remainingStmts = (do a <- stmts
                     return (a)) <|> (return [])

-- - Assign
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
assign :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
assign = do
    a <- idToken
    b <- assignToken
    c <- expression
    d <- semiColonToken
    s <- getState
    -- Check if the types are compatible
    if (not (compatible (getVariableType a s) c)) then fail "Type mismatch."
    else
        do
            updateState(updateVariable(((a, (cast (getVariableType a s) c)),
                        "m")))
            s <- getState
            liftIO (print s)
            return (a:b:[c])

-- - Print
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
printf :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
printf = do
    a <- printToken
    b <- openParenthesesToken
    c <- expression
    d <- closeParenthesesToken
    e <- semiColonToken
    liftIO (print (getValue c))
    return (a:b:c:d:[e])

-- - If statements
ifStmt :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
ifStmt = do
    a <- ifToken
    b <- openParenthesesToken
    c <- expression
    d <- closeParenthesesToken
    s <- getState
    e <- stmts
    -- if (getValue c) then do {
    --     e <- stmts
    --     f <- endIfToken
    --     return (a:b:c:[d] ++ e ++ [f])
    -- }
    f <- endIfToken
    return (a:b:c:[d] ++ e ++ [f])



-- -----------------------------------------------------------------------------
-- Starts parser
-- -----------------------------------------------------------------------------

-- - Parser
-- [Token]          Token list
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program initState "Error message" tokens
