-- Parser (Syntax Analyzer)
-- Version: 01/06/2017
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

-- Program Token
programToken = tokenPrim show updatePositon getToken where
    getToken (Program pos) = Just (Program pos)
    getToken _       = Nothing

-- End Token
endToken = tokenPrim show updatePositon getToken where
    getToken (End pos) = Just (End pos)
    getToken _   = Nothing

-- ID Token
idToken = tokenPrim show updatePositon getToken where
    getToken (Id x pos) = Just (Id x pos)
    getToken _      = Nothing

-- Colon Token
colonToken = tokenPrim show updatePositon getToken where
    getToken (Colon pos) = Just (Colon pos)
    getToken _     = Nothing

-- Semicolon Token
semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show updatePositon getToken where
    getToken (SemiColon pos) = Just (SemiColon pos)
    getToken _         = Nothing

-- Assign Token
assignToken = tokenPrim show updatePositon getToken where
    getToken (Assign pos) = Just (Assign pos)
    getToken _      = Nothing

-- Type Token
typeToken = tokenPrim show updatePositon getToken where
    getToken (Type x pos) = Just (Type x pos)
    getToken _        = Nothing

-- Nat Token
natToken = tokenPrim show updatePositon getToken where
    getToken (Nat x pos) = Just (Nat x pos)
    getToken _       = Nothing

-- Int Token
intToken = tokenPrim show updatePositon getToken where
    getToken (Int x pos) = Just (Int x pos)
    getToken _       = Nothing

-- Real Token
realToken = tokenPrim show updatePositon getToken where
    getToken (Real x pos) = Just (Real x pos)
    getToken _         = Nothing

-- Bool Token
boolToken = tokenPrim show updatePositon getToken where
    getToken (Bool x pos) = Just (Bool x pos)
    getToken _         = Nothing

-- Text Token
textToken = tokenPrim show updatePositon getToken where
    getToken (Text x pos) = Just (Text x pos)
    getToken _        = Nothing

-- Update position
updatePositon :: SourcePos -> Token -> [Token] -> SourcePos
updatePositon position _ (token:_) = position -- necessita melhoria
updatePositon position _ []        = position



-- -----------------------------------------------------------------------------
-- Parser to nonterminals
-- -----------------------------------------------------------------------------

program :: ParsecT [Token] [(Token, Token)] IO ([Token])
program = do
    a <- programToken
    b <- idToken
    c <- varDecls
    d <- stmts
    e <- endToken
    f <- idToken
    eof
    return (a:[b] ++ c ++ d ++ e:[f])

varDecls :: ParsecT [Token] [(Token, Token)] IO([Token])
varDecls = do
    first <- varDecl
    next  <- remaining_varDecls
    return (first ++ next)

varDecl :: ParsecT [Token] [(Token, Token)] IO([Token])
varDecl = do
    a <- typeToken
    b <- colonToken
    c <- idToken
    d <- semiColonToken
    updateState(symtableInsert (c, getDefaultValue a))
    s <- getState
    liftIO (print s)
    return (a:b:[c])

remaining_varDecls :: ParsecT [Token] [(Token, Token)] IO([Token])
remaining_varDecls = (do a <- varDecls
                         return (a)) <|> (return [])

-- Tratar outros stmts
stmts :: ParsecT [Token] [(Token, Token)] IO([Token])
stmts = do
    first <- assign
    next  <- remaining_stmts
    return (first ++ next)

assign :: ParsecT [Token] [(Token, Token)] IO([Token])
assign = do
    a <- idToken
    b <- assignToken
    c <- natToken <|> intToken <|> realToken <|> boolToken <|> textToken
    d <- semiColonToken
    updateState(symtableUpdate (a, c))
    s <- getState
    liftIO (print s)
    return (a:b:[c])

remaining_stmts :: ParsecT [Token] [(Token, Token)] IO([Token])
remaining_stmts = (do a <- stmts
                      return (a)) <|> (return [])



-- -----------------------------------------------------------------------------
-- Functions of the Symbol Table
-- -----------------------------------------------------------------------------

getDefaultValue :: Token -> Token
getDefaultValue (Type "Nat" pos) = Nat 0 pos
getDefaultValue (Type "Int" pos) = Int 0 pos
getDefaultValue (Type "Real" pos) = Real 0.0 pos
getDefaultValue (Type "Text" pos) = Text "" pos
-- getDefaultValue (Type "Univ") = Univ "\empty"
getDefaultValue (Type "Bool" pos) = Bool False pos
-- getDefaultValue (Type "Pointer") = Pointer 0.0
-- getDefaultValue (Type "Set[" <type> "]") = "Set[" <type> "]" "\empty"

symtableInsert :: (Token,Token) -> [(Token, Token)] -> [(Token, Token)]
symtableInsert symbol []  = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

symtableUpdate :: (Token,Token) -> [(Token, Token)] -> [(Token, Token)]
symtableUpdate _ [] = fail "Variable not found!"
symtableUpdate (id1, v1) ((id2, v2):t) =
    if id1 == id2 then (id1, v1) : t
    else (id2, v2) : symtableUpdate (id1, v1) t

symtableRemove :: (Token,Token) -> [(Token, Token)] -> [(Token, Token)]
symtableRemove _ [] = fail "Variable not found!"
symtableRemove (id1, v1) ((id2, v2):t) =
    if id1 == id2 then t
    else (id2, v2) : symtableRemove (id1, v1) t



-- -----------------------------------------------------------------------------
-- Starts parser
-- -----------------------------------------------------------------------------

-- Parser
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

-- Main
main :: IO ()
main = case unsafePerformIO (parser (getTokens "TestFiles/test-i.set")) of
    { Left err -> print err;
      Right ans -> print ans
    }
