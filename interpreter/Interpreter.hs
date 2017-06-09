-- Parser (Syntax Analyzer)
-- Version: 09/06/2017
module Interpreter where

-- Imports
import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Parsec
import Lexer
import Parser
import SymbolTable
import Expressions
import Types

-- -----------------------------------------------------------------------------
-- Parser to nonterminals
-- -----------------------------------------------------------------------------

-- - Program
program :: ParsecT [Token] [(Token, Token)] IO([Token])
program = do
    a <- programToken
    b <- idToken
    c <- varDecls
    d <- stmts
    e <- endToken
    f <- idToken
    eof
    return (a:[b] ++ c ++ d ++ e:[f])

-- - Variable declarations
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
varDecls :: ParsecT [Token] [(Token, Token)] IO([Token])
varDecls = do
    first <- varDecl
    next  <- remainingVarDecls
    return (first ++ next)

-- - Variable declaration
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
varDecl :: ParsecT [Token] [(Token, Token)] IO([Token])
varDecl = do
    a <- typeToken
    b <- colonToken
    c <- idToken
    d <- semiColonToken
    updateState(symtableInsert(c, getDefaultValue(a)))
    s <- getState
    liftIO (print s)
    return (a:b:[c])

-- - Variable declaration remaining
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
remainingVarDecls :: ParsecT [Token] [(Token, Token)] IO([Token])
remainingVarDecls = (do a <- varDecls
                        return (a)) <|> (return [])

-- - Statements
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
stmts :: ParsecT [Token] [(Token, Token)] IO([Token])
stmts = do
    first <- assign <|> printS
    next  <- remainingStmts
    return (first ++ next)

-- - Statements remaining
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
remainingStmts :: ParsecT [Token] [(Token, Token)] IO([Token])
remainingStmts = (do a <- stmts
                     return (a)) <|> (return [])

-- - Assign
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
assign :: ParsecT [Token] [(Token, Token)] IO([Token])
assign = do
    a <- idToken
    b <- assignToken
    c <- expression
    d <- semiColonToken
    s <- getState
    -- Check if the types are compatible
    if (not (compatible (getType a s) c)) then fail "Type mismatch."
    else
        do
            updateState(symtableUpdate(a, (cast (getType a s) c)))
            s <- getState
            liftIO (print s)
            return (a:b:[c])

-- - Print
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
printS :: ParsecT [Token] [(Token, Token)] IO([Token])
printS = do
    a <- printToken
    b <- openParenthesesToken
    c <- expression
    d <- closeParenthesesToken
    e <- semiColonToken
    liftIO (print (getValue c))
    return (a:b:c:d:[e])

-- - Cast
-- Token  Variable type
-- Token  Expression type
-- Return New expression type
cast :: Token -> Token -> Token
cast (Nat _ _)   (Nat i p) = if i < 0 then error "Invalid assignment."
                             else Nat i p
cast (Int _ _)   (Nat i p) = if i < 0 then Int i p
                             else Nat i p
cast (Int _ _)   (Int i p) = Int i p
cast (Real _ _)  (Nat i p) = let x = integerToFloat(i) in Real x p
cast (Real _ _)  (Int i p) = let x = integerToFloat(i) in Real x p
cast (Real _ _) (Real i p) = Real i p
cast (Bool _ _) (Bool i p) = Bool i p
cast (Text _ _) (Text i p) = Text i p
cast _ _ = error "Invalid cast."

-- - Check whether types are compatible
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
compatible :: Token -> Token -> Bool
compatible (Nat _ _)   (Nat _ _) = True
compatible (Int _ _)   (Int _ _) = True
compatible (Int _ _)   (Nat _ _) = True
compatible (Real _ _) (Real _ _) = True
compatible (Real _ _)  (Int _ _) = True
compatible (Real _ _)  (Nat _ _) = True
compatible (Bool _ _) (Bool _ _) = True
-- compatible (Univ _ _) (Univ _ _) = True
compatible (Text _ _) (Text _ _) = True
-- compatible (Pointer _ _) (Pointer _ _) = True
compatible _ _ = False



-- -----------------------------------------------------------------------------
-- Starts parser
-- -----------------------------------------------------------------------------

-- - Parser
-- [Token]          Token list
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens
