-- Parser (Syntax Analyzer)
-- Version: 10/05/2017
-- Author : Breno Viana
-- module Parser where
module Main (main) where

-- Imports
import Control.Monad.IO.Class
import System.IO.Unsafe
import Data.List
import Text.Parsec
import Lexer

-- -----------------------------------------------------------------------------
-- Parser to Tokens
-- -----------------------------------------------------------------------------

-- Describe function
programToken = tokenPrim show updatePositon getToken where
    getToken Program = Just Program
    getToken _       = Nothing

-- Describe function
idToken = tokenPrim show updatePositon getToken where
    getToken (Id x) = Just (Id x)
    getToken _      = Nothing

-- Describe function
varToken = tokenPrim show updatePositon getToken where
    getToken Var = Just Var
    getToken _   = Nothing

-- Describe function
beginToken = tokenPrim show updatePositon getToken where
    getToken Begin = Just Begin
    getToken _     = Nothing

-- Describe function
endToken = tokenPrim show updatePositon getToken where
    getToken End = Just End
    getToken _   = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
-- Describe function
semiColonToken = tokenPrim show updatePositon getToken where
    getToken SemiColon = Just SemiColon
    getToken _         = Nothing

-- Describe function
colonToken = tokenPrim show updatePositon getToken where
    getToken Colon = Just Colon
    getToken _     = Nothing

-- Describe function
assignToken = tokenPrim show updatePositon getToken where
    getToken Assign = Just Assign
    getToken _      = Nothing

-- Describe function
intToken = tokenPrim show updatePositon getToken where
    getToken (Int x) = Just (Int x)
    getToken _       = Nothing

-- Describe function
typeToken = tokenPrim show updatePositon getToken where
    getToken (Type x) = Just (Type x)
    getToken _        = Nothing

--
updatePositon :: SourcePos -> Token -> [Token] -> SourcePos
updatePositon position _ (token:_) = position -- necessita melhoria
updatePositon position _ []        = position



-- -----------------------------------------------------------------------------
-- Parser to nonterminals
-- -----------------------------------------------------------------------------

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
    a <- programToken
    b <- idToken
    c <- varToken
    d <- varDecl
    e <- beginToken
    f <- stmts
    g <- endToken
    eof
    return (a:b:[c] ++ d++ [e] ++ f ++ [g])

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
    a <- idToken
    b <- colonToken
    c <- typeToken
    updateState(symtable_insert (a, get_default_value c))
    s <- getState
    liftIO (print s)
    return (a:b:[c])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
    first <- assign
    next <- remaining_stmts
    return (first ++ next)

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
    a <- idToken
    b <- assignToken
    c <- intToken
    updateState(symtable_update (a, c))
    s <- getState
    liftIO (print s)
    return (a:b:[c])

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_stmts = (do a <- semiColonToken
    b <- assign
    return (a:b)) <|> (return [])



-- -----------------------------------------------------------------------------
-- Functions of the Symbol Table
-- -----------------------------------------------------------------------------

get_default_value :: Token -> Token
get_default_value (Type "int") = Int 0

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "Variable not found!"
symtable_update (id1, v1) ((id2, v2):t) =
    if id1 == id2 then (id1, v1) : t
    else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "Variable not found!"
symtable_remove (id1, v1) ((id2, v2):t) =
    if id1 == id2 then t
    else (id2, v2) : symtable_remove (id1, v1) t



-- -----------------------------------------------------------------------------
-- Starts parser
-- -----------------------------------------------------------------------------

-- Parser
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

-- Main
main :: IO ()
main = case unsafePerformIO (parser (getTokens "LexerMaker/test.set")) of
    { Left err -> print err;
      Right ans -> print ans
    }
