-- Parser (Syntax Analyzer)
-- Version: 11/06/2017
module Parser where

-- External imports
import Control.Monad.IO.Class
import Text.Parsec
-- Internal imports
import Lexer
import Types
import State

-- -----------------------------------------------------------------------------
-- Parser to Tokens
-- -----------------------------------------------------------------------------

-- - Program Token
programToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
programToken = tokenPrim show updatePositon getToken where
    getToken (Program pos) = Just (Program pos)
    getToken _             = Nothing

-- - End Token
endToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
endToken = tokenPrim show updatePositon getToken where
    getToken (End pos) = Just (End pos)
    getToken _         = Nothing

-- - ID Token
idToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
idToken = tokenPrim show updatePositon getToken where
    getToken (Id x pos) = Just (Id x pos)
    getToken _          = Nothing

-- - Colon Token
colonToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
colonToken = tokenPrim show updatePositon getToken where
    getToken (Colon pos) = Just (Colon pos)
    getToken _           = Nothing

-- - Semicolon Token
semiColonToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
semiColonToken = tokenPrim show updatePositon getToken where
    getToken (SemiColon pos) = Just (SemiColon pos)
    getToken _               = Nothing

-- - Assign Token
assignToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
assignToken = tokenPrim show updatePositon getToken where
    getToken (Assign pos) = Just (Assign pos)
    getToken _            = Nothing



-- --------------------------------------------------------
-- Type tokens
-- --------------------------------------------------------

-- - Type Token
typeToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
typeToken = tokenPrim show updatePositon getToken where
    getToken (Type x pos) = Just (Type x pos)
    getToken _            = Nothing

-- - Nat Token
natToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
natToken = tokenPrim show updatePositon getToken where
    getToken (Nat x pos) = if x < 0 then error "Invalid assignment."
                           else Just (Nat x pos)
    getToken _           = Nothing

-- - Int Token
intToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
intToken = tokenPrim show updatePositon getToken where
    getToken (Nat x pos) = Just (Int x pos)
    getToken (Int x pos) = Just (Int x pos)
    getToken _           = Nothing

-- - Real Token
realToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
realToken = tokenPrim show updatePositon getToken where
    getToken (Nat  x pos) = let y = integerToFloat(x) in Just (Real y pos)
    getToken (Int  x pos) = let y = integerToFloat(x) in Just (Real y pos)
    getToken (Real x pos) = Just (Real x pos)
    getToken _            = Nothing

-- - Bool Token
boolToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
boolToken = tokenPrim show updatePositon getToken where
    getToken (Bool x pos) = Just (Bool x pos)
    getToken _            = Nothing

-- - Univ Token
-- univToken = tokenPrim show updatePositon getToken where
--     getToken (Univ x pos) = Just (Univ x pos)
--     getToken _        = Nothing

-- - Text Token
textToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
textToken = tokenPrim show updatePositon getToken where
    getToken (Text x pos) = let y = removeQuote x in Just (Text y pos)
    getToken _            = Nothing



-- --------------------------------------------------------
-- Operator tokens
-- --------------------------------------------------------

-- - Addition Token
additionToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
additionToken = tokenPrim show updatePositon getToken where
    getToken (Addition p) = Just (Addition p)
    getToken _            = Nothing

-- - Subtraction Token
subtractionToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
subtractionToken = tokenPrim show updatePositon getToken where
    getToken (Subtraction p) = Just (Subtraction p)
    getToken _               = Nothing

-- - Multiplication Token
multiplicationToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
multiplicationToken = tokenPrim show updatePositon getToken where
    getToken (Multiplication p) = Just (Multiplication p)
    getToken _                  = Nothing

-- - Division Token
divisionToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
divisionToken = tokenPrim show updatePositon getToken where
    getToken (Division p) = Just (Division p)
    getToken _       = Nothing

-- - Equality Token
equalityToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
equalityToken = tokenPrim show updatePositon getToken where
    getToken (Equality p) = Just (Equality p)
    getToken _            = Nothing

-- - Greater Token
greaterToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
greaterToken = tokenPrim show updatePositon getToken where
    getToken (Greater p) = Just (Greater p)
    getToken _           = Nothing

-- - GreaterOrEqual Token
greaterOrEqualToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
greaterOrEqualToken = tokenPrim show updatePositon getToken where
    getToken (GreaterOrEqual p) = Just (GreaterOrEqual p)
    getToken _                  = Nothing

-- - Smaller Token
smallerToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
smallerToken = tokenPrim show updatePositon getToken where
    getToken (Smaller p) = Just (Smaller p)
    getToken _           = Nothing

-- - SmallerOrEqual Token
smallerOrEqualToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
smallerOrEqualToken = tokenPrim show updatePositon getToken where
    getToken (SmallerOrEqual p) = Just (SmallerOrEqual p)
    getToken _                  = Nothing
    
-- - Denial Token
denialToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
denialToken = tokenPrim show updatePositon getToken where
    getToken (Denial p) = Just (Denial p)
    getToken _                  = Nothing
    
-- - And Token
andToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
andToken = tokenPrim show updatePositon getToken where
    getToken (And p) = Just (And p)
    getToken _       = Nothing
    
-- - Or Token
orToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
orToken = tokenPrim show updatePositon getToken where
    getToken (Or p) = Just (Or p)
    getToken _      = Nothing



-- --------------------------------------------------------
-- Native function tokens
-- --------------------------------------------------------

-- - Print Token
printToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
printToken = tokenPrim show updatePositon getToken where
    getToken (Print p) = Just (Print p)
    getToken _         = Nothing

-- - Input Token
inputToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
inputToken = tokenPrim show updatePositon getToken where
    getToken (Input p) = Just (Input p)
    getToken _         = Nothing

-- - If Token
ifToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
ifToken = tokenPrim show updatePositon getToken where
    getToken (If p) = Just (If p)
    getToken _      = Nothing

-- - Else Token
elseToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
elseToken = tokenPrim show updatePositon getToken where
    getToken (Else p) = Just (Else p)
    getToken _        = Nothing

-- - Else_If Token
elseIfToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
elseIfToken = tokenPrim show updatePositon getToken where
    getToken (Else_If p) = Just (Else_If p)
    getToken _           = Nothing

-- - End_If Token
endIfToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
endIfToken = tokenPrim show updatePositon getToken where
    getToken (End_If p) = Just (End_If p)
    getToken _          = Nothing

-- - While Token
whileToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
whileToken = tokenPrim show updatePositon getToken where
    getToken (While p) = Just (While p)
    getToken _         = Nothing

-- - End_While Token
endWhileToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
endWhileToken = tokenPrim show updatePositon getToken where
    getToken (End_While p) = Just (End_While p)
    getToken _             = Nothing

-- --------------------------------------------------------
-- Other tokens
-- --------------------------------------------------------

-- - OpenParentheses Token
openParenthesesToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
openParenthesesToken = tokenPrim show updatePositon getToken where
    getToken (Open_Parentheses p) = Just (Open_Parentheses p)
    getToken _                    = Nothing

-- - CloseParentheses Token
closeParenthesesToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
closeParenthesesToken = tokenPrim show updatePositon getToken where
    getToken (Close_Parentheses p) = Just (Close_Parentheses p)
    getToken _                     = Nothing

-- - CloseParentheses Token
ignoreToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
ignoreToken = tokenPrim show updatePositon getToken where
    getToken t = Just t
    getToken _ = Nothing

-- - CloseParentheses Token
-- arrayToken :: ParsecT [Token] (Scope, [Var], [Statement]) IO(Token)
-- arrayToken = tokenPrim show updatePositon getToken where
--     getToken (Array p) = Just (Array p)
--     getToken _         = Nothing



-- -----------------------------------------------------------------------------
-- Other necessary functions
-- -----------------------------------------------------------------------------

-- - Update position
-- SourcePos Position
-- Token     Token
-- [Token]   All tokens
-- Return    Updated position
updatePositon :: SourcePos -> Token -> [Token] -> SourcePos
updatePositon position _ (token:_) = position -- necessita melhoria
updatePositon position _ []        = position
