-- Interpreter
-- Version: 11/06/2017
module Interpreter where

-- External imports
import Data.List
import Text.Parsec
import Control.Monad
import System.IO.Unsafe
import Control.Monad.IO.Class
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

-- - Variable declaration remaining
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
remainingVarDecls :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
remainingVarDecls = (do a <- varDecls
                        return (a)) <|> (return [])

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
    -- updateState(updateVariable(((a, (cast (getVariableType a s) c))
    updateState(insertVariable((c, getDefaultValue(a)), "m"))
    return (a:b:[c])

-- - Statements
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
stmts :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
stmts = do
    first <- assign <|> varDecls <|> printf <|> inputf <|> ifStmt <?> "expecting"
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
            -- liftIO (print s)
            return (a:b:[c])



-- -----------------------------------------------------------------------------
-- IF STATEMENT
-- -----------------------------------------------------------------------------

-- - If
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
ifStmt :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
ifStmt = do
    s <- getState
    a <- ifToken
    b <- openParenthesesToken
    c <- expression
    d <- closeParenthesesToken
    updateState(insertScope(("if" ++ (show (getScopeLength s)))))
    if ((getValue c) == "True") then do
        e <- stmts
        let loop = do
            f <- ignoreToken
            when (((columnEndStmt f) /= (columnIfStmt a))) loop
        loop
        updateState(removeScope(("if" ++ (show (getScopeLength s)))))
        return (a:b:c:[d] ++ e)
    else do
        updateState(removeScope(("if" ++ (show (getScopeLength s)))))
        updateState(insertScope(("if" ++ (show (getScopeLength s)))))
        bs <- getInput
        let loop = do
            f <- ignoreToken
            when (((columnElseStmt f) /= (columnIfStmt a))
                && ((columnElseIfStmt f) /= (columnIfStmt a))
                && ((columnEndStmt f) /= (columnIfStmt a))) loop
        loop
        af <- getInput
        setInput (let y:x = reverse (bs \\ af) in y:af)
        bs <- getInput
        e <- endIfToken <|> elseToken <|> elseIfToken
        if ((checkElseStmt e) == "True") then do
            e <- stmts
            f <- endIfToken
            updateState(removeScope(("if" ++ (show (getScopeLength s)))))
            return (a:b:c:[d])
        else
            if ((checkElseIfStmt e) == "True") then do
                af <- getInput
                setInput (let y:x = reverse (bs \\ af) in y:af)
                f <- elseIfStmt
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d])
            else do
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d])

-- - If
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
elseIfStmt :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
elseIfStmt = do
    s <- getState
    a <- elseIfToken
    b <- openParenthesesToken
    c <- expression
    d <- closeParenthesesToken
    updateState(insertScope(("if" ++ (show (getScopeLength s)))))
    if ((getValue c) == "True") then do
        e <- stmts
        let loop = do
            f <- ignoreToken
            when (((columnEndStmt f) /= (columnElseIfStmt a))) loop
        loop
        updateState(removeScope(("if" ++ (show (getScopeLength s)))))
        return (a:b:[c] ++ e)
    else do
        updateState(removeScope(("if" ++ (show (getScopeLength s)))))
        updateState(insertScope(("if" ++ (show (getScopeLength s)))))
        bs <- getInput
        let loop = do
            f <- ignoreToken
            when (((columnElseStmt f) /= (columnIfStmt a))
                && ((columnElseIfStmt f) /= (columnIfStmt a))
                && ((columnEndStmt f) /= (columnIfStmt a))) loop
        loop
        af <- getInput
        setInput (let y:x = reverse (bs \\ af) in y:af)
        e <- endIfToken <|> elseToken <|> elseIfToken
        bs <- getInput
        liftIO (print af)
        if ((checkElseStmt e) == "True") then do
            e <- stmts
            f <- endIfToken
            updateState(removeScope(("if" ++ (show (getScopeLength s)))))
            return (a:b:c:[d])
        else
            if ((checkElseIfStmt e) == "True") then do
                af <- getInput
                setInput (let y:x = reverse (bs \\ af) in y:af)
                f <- elseIfStmt
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d])
            else do
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d])



-- -----------------------------------------------------------------------------
-- WHILE STATEMENT
-- -----------------------------------------------------------------------------





-- -----------------------------------------------------------------------------
-- OTHER STATEMENTS
-- -----------------------------------------------------------------------------

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
    liftIO (putStrLn ((getValue c)))
    return (a:b:c:d:[e])

-- - Input
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
inputf :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
inputf = do
    a <- inputToken
    b <- idToken
    c <- semiColonToken
    d <- liftIO $ getLine
    liftIO (print d)
    s <- getState
    updateState(updateVariable((b, (cast (getVariableType b s) (Text (show d) (let (Id _ y) = b in y)))), "m"))
    return (a:b:[c])

-- -----------------------------------------------------------------------------
-- Starts parser
-- -----------------------------------------------------------------------------

-- - Parser
-- [Token]          Token list
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program initState "Error message" tokens
