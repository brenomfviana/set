-- Parser (Syntax Analyzer)
-- Version: 01/06/2017
-- module Parser where
module Main (main) where

-- Imports
import Control.Monad.IO.Class
import System.IO.Unsafe
import Text.Parsec
import Lexer
import SymbolTable

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

-- Univ Token
-- univToken = tokenPrim show updatePositon getToken where
--     getToken (Univ x pos) = Just (Univ x pos)
--     getToken _        = Nothing

-- Text Token
textToken = tokenPrim show updatePositon getToken where
    getToken (Text x pos) = Just (Text x pos)
    getToken _        = Nothing

-- Addition Token
additionToken = tokenPrim show updatePositon getToken where
  getToken (Addition p) = Just (Addition p)
  getToken _       = Nothing

-- Update position
updatePositon :: SourcePos -> Token -> [Token] -> SourcePos
updatePositon position _ (token:_) = position -- necessita melhoria
updatePositon position _ []        = position


-- -----------------------------------------------------------------------------
-- Parser to nonterminals
-- -----------------------------------------------------------------------------

--
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

--
varDecls :: ParsecT [Token] [(Token, Token)] IO([Token])
varDecls = do
    first <- varDecl
    next  <- remainingVarDecls
    return (first ++ next)

--
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

--
remainingVarDecls :: ParsecT [Token] [(Token, Token)] IO([Token])
remainingVarDecls = (do a <- varDecls
                        return (a)) <|> (return [])

--
stmts :: ParsecT [Token] [(Token, Token)] IO([Token])
stmts = do
    first <- assign
    next  <- remainingStmts
    return (first ++ next)

--
assign :: ParsecT [Token] [(Token, Token)] IO([Token])
assign = do
    a <- idToken
    b <- assignToken
    c <- expression
    d <- semiColonToken
    s <- getState
    --
    if (not (compatible (getType a s) c)) then fail "Type mismatch"
    else
      do
        updateState(symtableUpdate(a, c))
        s <- getState
        liftIO (print s)
        return (a:b:[c])



-- -----------------------------------------------------------------------------
-- Type checking
-- -----------------------------------------------------------------------------

-- Get default value of different types
getDefaultValue :: Token -> Token
getDefaultValue (Type "Nat" pos) = Nat 0 pos
getDefaultValue (Type "Int" pos) = Int 0 pos
getDefaultValue (Type "Real" pos) = Real 0.0 pos
getDefaultValue (Type "Bool" pos) = Bool False pos
-- getDefaultValue (Type "Univ" pos) = Univ "" pos
getDefaultValue (Type "Text" pos) = Text "" pos
-- getDefaultValue (Type "Pointer") = Pointer 0.0
-- getDefaultValue (Type "Set[" <type> "]") = "Set[" <type> "]" "\empty"

-- Get type
getType :: Token -> [(Token, Token)] -> Token
getType _ [] = error "Variable not found"
getType (Id id1 p1) ((Id id2 _, value):t) = if id1 == id2 then value
                                            else getType (Id id1 p1) t

-- Check whether types are compatible
compatible :: Token -> Token -> Bool
compatible (Nat _ _) (Nat _ _) = True
compatible (Int _ _) (Int _ _) = True
compatible (Real _ _) (Real _ _) = True
compatible (Bool _ _) (Bool _ _) = True
-- compatible (Univ _ _) (Univ _ _) = True
compatible (Text _ _) (Text _ _) = True
-- compatible (Pointer _ _) (Pointer _ _) = True
compatible _ _ = False



-- -----------------------------------------------------------------------------
-- funções para o avaliador de expressões
-- -----------------------------------------------------------------------------

--
expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> una_expression

--
una_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
una_expression = do
                   a <- natToken <|> intToken <|> realToken <|> boolToken <|> textToken
                   return (a)

--
bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   a <- natToken <|> intToken <|> realToken
                   b <- additionToken
                   c <- natToken <|> intToken <|> realToken
                   return (eval a b c)

--
eval :: Token -> Token -> Token -> Token
eval (Nat x p) (Addition _) (Nat y _) = Nat (x + y) p
eval (Int x p) (Addition _) (Int y _) = Int (x + y) p
eval (Real x p) (Addition _) (Real y _) = Real (x + y) p

--
remainingStmts :: ParsecT [Token] [(Token, Token)] IO([Token])
remainingStmts = (do a <- stmts
                     return (a)) <|> (return [])



-- -----------------------------------------------------------------------------
-- Starts parser
-- -----------------------------------------------------------------------------

-- Parser
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

-- Main
main :: IO ()
main = do
    -- print (getTokens "TestFiles/test-i.set")
    case unsafePerformIO (parser (getTokens "TestFiles/test-i.set")) of
    { Left err -> print err;
      Right ans -> print ans
    }
