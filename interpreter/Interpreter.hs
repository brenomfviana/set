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
import Keywords
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
    -- Update scope
    updateState(insertScope(getIdName b))
    c <- stmts
    d <- endToken
    -- Update scope
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
    -- Check if the variable already exists
    -- updateState(updateVariable(((a, (cast (getVariableType a s) c))
    -- Add the declared variable
    updateState(insertVariable((c, getDefaultValue(a)), "m"))
    return (a:b:[c])

-- - Statements
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
stmts :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
stmts = do
    first <- assign <|> varDecls <|> printf <|> inputf <|> ifStmt
             <?> "expecting"
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
    -- Calculates the expression
    c <- expression
    d <- semiColonToken
    s <- getState
    -- Check if the types are compatible
    if (not (compatible (getVariableType a s) c)) then fail "Type mismatch."
    else
        do
            -- Update variable value
            updateState(updateVariable(((a, (cast (getVariableType a s) c)),
                        "m")))
            -- s <- getState
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
    -- Calculates the expression
    c <- expression
    d <- closeParenthesesToken
    -- Update scope
    updateState(insertScope(("if" ++ (show (getScopeLength s)))))
    -- Check if the expression is true
    if ((getValue c) == "True") then do
        -- Get the next statement
        e <- ignoreToken
        af <- getInput
        -- Add back the last readed statement
        setInput (e:af)
        -- Check if the token is a END_IF
        if (((checkEndIfStmt e) == "True")) then do
            e <- endIfToken
            -- Update scope
            updateState(removeScope(("if" ++ (show (getScopeLength s)))))
            return (a:b:c:d:[e])
        else
            -- Check if the token is a ELSE or ELSE_IF
            if (((checkElseIfStmt e) == "True")
                    || ((checkElseStmt e) == "True")) then do
                -- Ignore statements
                bf <- getInput
                let loop = do
                    f <- ignoreToken
                    when (((columnEndIfStmt f) /= (columnIfStmt a))) loop
                loop
                af <- getInput
                -- Update scope
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d] ++ (bf \\ af))
            else do
                -- Run statements
                e <- stmts
                -- Ignore other statements
                bf <- getInput
                let loop = do
                    f <- ignoreToken
                    when (((columnEndIfStmt f) /= (columnIfStmt a))) loop
                loop
                af <- getInput
                -- Update scope
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d] ++ e ++ (bf \\ af))
    else do
        -- Update scope
        updateState(removeScope(("if" ++ (show (getScopeLength s)))))
        -- Get the next statement
        e <- ignoreToken
        af <- getInput
        -- Add back the last readed statement
        setInput (e:af)
        -- Check if the token is a END_IF
        if (((checkEndIfStmt e) == "True")) then do
            e <- endIfToken
            return (a:b:c:d:[e])
        else do
            -- Update scope
            updateState(insertScope(("if" ++ (show (getScopeLength s)))))
            -- Ignore other statements
            bf <- getInput
            let loop = do
                f <- ignoreToken
                when (((columnElseStmt f) /= (columnIfStmt a))
                    && ((columnElseIfStmt f) /= (columnIfStmt a))
                    && ((columnEndIfStmt f) /= (columnIfStmt a))) loop
            loop
            af <- getInput
            -- Add back the last readed statement
            setInput (let y:x = reverse (bf \\ af) in y:af)
            bf <- getInput
            e <- endIfToken <|> elseToken <|> elseIfToken
            -- Check if the token is a ELSE
            if ((checkElseStmt e) == "True") then do
                -- Get the next statement
                e <- ignoreToken
                af <- getInput
                -- Add back the last readed statement
                setInput (e:af)
                -- Check if the token is a END_IF
                if (((checkEndIfStmt e) == "True")) then do
                    e <- endIfToken
                    return (a:b:c:d:[e])
                else do
                    e <- stmts
                    f <- endIfToken
                    -- Update scope
                    updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                    return (a:b:c:[d] ++ e ++ [f])
            else
                -- Check if the token is a ELSE_IF
                if ((checkElseIfStmt e) == "True") then do
                    af <- getInput
                    -- Add back the last readed statement
                    setInput (let y:x = reverse (bf \\ af) in y:af)
                    f <- elseIfStmt
                    -- Update scope
                    updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                    return (a:b:c:[d])
                else do
                    -- Update scope
                    updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                    return (a:b:c:[d])

-- - Else if
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
elseIfStmt :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
elseIfStmt = do
    s <- getState
    a <- elseIfToken
    b <- openParenthesesToken
    -- Calculates the expression
    c <- expression
    d <- closeParenthesesToken
    -- Update scope
    updateState(insertScope(("if" ++ (show (getScopeLength s)))))
    -- Check if the expression is true
    if ((getValue c) == "True") then do
        -- Get the next statement
        e <- ignoreToken
        af <- getInput
        -- Add back the last readed statement
        setInput (e:af)
        -- Check if the token is a END_IF
        if (((checkEndIfStmt e) == "True")) then do
            e <- endIfToken
            -- Update scope
            updateState(removeScope(("if" ++ (show (getScopeLength s)))))
            return (a:b:c:d:[e])
        else
            -- Check if the token is a ELSE or ELSE_IF
            if (((checkElseIfStmt e) == "True")
                    || ((checkElseStmt e) == "True")) then do
                    -- Ignore other statements
                bf <- getInput
                let loop = do
                    f <- ignoreToken
                    when (((columnEndIfStmt f) /= (columnElseIfStmt a))) loop
                loop
                af <- getInput
                -- Update scope
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d] ++ (bf \\ af))
            else do
                -- Run statements
                e <- stmts
                -- Ignore other statements
                bf <- getInput
                let loop = do
                    f <- ignoreToken
                    when (((columnEndIfStmt f) /= (columnElseIfStmt a))) loop
                loop
                af <- getInput
                -- Update scope
                updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                return (a:b:c:[d] ++ e ++ (bf \\ af))
    else do
        -- Update scope
        updateState(removeScope(("if" ++ (show (getScopeLength s)))))
        -- Get the next statement
        e <- ignoreToken
        af <- getInput
        -- Add back the last readed statement
        setInput (e:af)
        -- Check if the token is a END_IF
        if (((checkEndIfStmt e) == "True")) then do
            e <- endIfToken
            return (a:b:c:d:[e])
        else do
            -- Update scope
            updateState(insertScope(("if" ++ (show (getScopeLength s)))))
            -- Ignore other statements
            bf <- getInput
            let loop = do
                f <- ignoreToken
                when (((columnElseStmt f) /= (columnIfStmt a))
                    && ((columnElseIfStmt f) /= (columnIfStmt a))
                    && ((columnEndIfStmt f) /= (columnIfStmt a))) loop
            loop
            af <- getInput
            -- Add back the last readed statement
            setInput (let y:x = reverse (bf \\ af) in y:af)
            bf <- getInput
            e <- endIfToken <|> elseToken <|> elseIfToken
            -- Check if the token is a ELSE
            if ((checkElseStmt e) == "True") then do
                -- Get the next statement
                e <- ignoreToken
                af <- getInput
                -- Add back the last readed statement
                setInput (e:af)
                -- Check if the token is a END_IF
                if (((checkEndIfStmt e) == "True")) then do
                    e <- endIfToken
                    return (a:b:c:d:[e])
                else do
                    e <- stmts
                    f <- endIfToken
                    -- Update scope
                    updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                    return (a:b:c:[d])
            else
                -- Check if the token is a ELSE_IF
                if ((checkElseIfStmt e) == "True") then do
                    af <- getInput
                    -- Add back the last readed statement
                    setInput (let y:x = reverse (bf \\ af) in y:af)
                    f <- elseIfStmt
                    -- Update scope
                    updateState(removeScope(("if" ++ (show (getScopeLength s)))))
                    return (a:b:c:[d])
                else do
                    -- Update scope
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
    -- Calculates the expression
    c <- expression
    d <- closeParenthesesToken
    e <- semiColonToken
    -- Prints in terminal
    liftIO (putStrLn ((getValue c)))
    return (a:b:c:d:[e])

-- - Input
-- ParsecT                     ParsecT
-- [Token]                     Token list
-- (Scope, [Var], [Statement]) State
inputf :: ParsecT [Token] (Scope, [Var], [Statement]) IO([Token])
inputf = do
    a <- inputToken
    b <- openParenthesesToken
    c <- idToken
    d <- closeParenthesesToken
    e <- semiColonToken
    f <- liftIO $ getLine
    s <- getState
    -- Update variable value
    updateState(updateVariable((c,
        (inputCast (getVariableType c s)
            (Text (show f) (let (Id _ y) = c in y)))), "m"))
    return (a:b:[c])



-- -----------------------------------------------------------------------------
-- Starts parser
-- -----------------------------------------------------------------------------

-- - Parser
-- [Token]          Token list
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program initState "Error message" tokens
