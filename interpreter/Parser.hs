-- Parser (Syntax Analyzer)
-- Version: 09/06/2017
module Parser where

-- Imports
import Control.Monad.IO.Class
import Text.Parsec
import Lexer

-- -----------------------------------------------------------------------------
-- Parser to Tokens
-- -----------------------------------------------------------------------------

-- - Program Token
programToken :: ParsecT [Token] st IO(Token)
programToken = tokenPrim show updatePositon getToken where
    getToken (Program pos) = Just (Program pos)
    getToken _             = Nothing

-- - End Token
endToken :: ParsecT [Token] st IO(Token)
endToken = tokenPrim show updatePositon getToken where
    getToken (End pos) = Just (End pos)
    getToken _         = Nothing

-- - ID Token
idToken :: ParsecT [Token] st IO(Token)
idToken = tokenPrim show updatePositon getToken where
    getToken (Id x pos) = Just (Id x pos)
    getToken _          = Nothing

-- - Colon Token
colonToken :: ParsecT [Token] st IO(Token)
colonToken = tokenPrim show updatePositon getToken where
    getToken (Colon pos) = Just (Colon pos)
    getToken _           = Nothing

-- - Semicolon Token
semiColonToken :: ParsecT [Token] st IO(Token)
semiColonToken = tokenPrim show updatePositon getToken where
    getToken (SemiColon pos) = Just (SemiColon pos)
    getToken _               = Nothing

-- - Assign Token
assignToken :: ParsecT [Token] st IO(Token)
assignToken = tokenPrim show updatePositon getToken where
    getToken (Assign pos) = Just (Assign pos)
    getToken _            = Nothing



-- --------------------------------------------------------
-- Type tokens
-- --------------------------------------------------------

-- - Type Token
typeToken :: ParsecT [Token] st IO(Token)
typeToken = tokenPrim show updatePositon getToken where
    getToken (Type x pos) = Just (Type x pos)
    getToken _            = Nothing

-- - Nat Token
natToken :: ParsecT [Token] st IO(Token)
natToken = tokenPrim show updatePositon getToken where
    getToken (Nat x pos) = if x < 0 then error "Invalid assignment."
                           else Just (Nat x pos)
    getToken _           = Nothing

-- - Int Token
intToken :: ParsecT [Token] st IO(Token)
intToken = tokenPrim show updatePositon getToken where
    getToken (Nat x pos) = Just (Int x pos)
    getToken (Int x pos) = Just (Int x pos)
    getToken _           = Nothing

-- - Real Token
realToken :: ParsecT [Token] st IO(Token)
realToken = tokenPrim show updatePositon getToken where
    getToken (Nat  x pos) = let y = integerToFloat(x) in Just (Real y pos)
    getToken (Int  x pos) = let y = integerToFloat(x) in Just (Real y pos)
    getToken (Real x pos) = Just (Real x pos)
    getToken _            = Nothing

-- - Bool Token
boolToken :: ParsecT [Token] st IO(Token)
boolToken = tokenPrim show updatePositon getToken where
    getToken (Bool x pos) = Just (Bool x pos)
    getToken _            = Nothing

-- - Univ Token
-- univToken = tokenPrim show updatePositon getToken where
--     getToken (Univ x pos) = Just (Univ x pos)
--     getToken _        = Nothing

-- - Text Token
textToken :: ParsecT [Token] st IO(Token)
textToken = tokenPrim show updatePositon getToken where
    getToken (Text x pos) = Just (Text x pos)
    getToken _            = Nothing



-- --------------------------------------------------------
-- Operator



-- --------------------------------------------------------
-- Expression tokens
-- -------------------------------------------------------- tokens
-- --------------------------------------------------------

-- - Addition Token
additionToken :: ParsecT [Token] st IO(Token)
additionToken = tokenPrim show updatePositon getToken where
    getToken (Addition p) = Just (Addition p)
    getToken _            = Nothing

-- - Subtraction Token
subtractionToken :: ParsecT [Token] st IO(Token)
subtractionToken = tokenPrim show updatePositon getToken where
    getToken (Subtraction p) = Just (Subtraction p)
    getToken _               = Nothing

-- - Multiplication Token
multiplicationToken :: ParsecT [Token] st IO(Token)
multiplicationToken = tokenPrim show updatePositon getToken where
    getToken (Multiplication p) = Just (Multiplication p)
    getToken _                  = Nothing

-- - Division Token
divisionToken :: ParsecT [Token] st IO(Token)
divisionToken = tokenPrim show updatePositon getToken where
    getToken (Division p) = Just (Division p)
    getToken _       = Nothing

-- - Equality Token
equalityToken :: ParsecT [Token] st IO(Token)
equalityToken = tokenPrim show updatePositon getToken where
    getToken (Equality p) = Just (Equality p)
    getToken _            = Nothing

-- - Greater Token
greaterToken :: ParsecT [Token] st IO(Token)
greaterToken = tokenPrim show updatePositon getToken where
    getToken (Greater p) = Just (Greater p)
    getToken _           = Nothing

-- - GreaterOrEqual Token
greaterOrEqualToken :: ParsecT [Token] st IO(Token)
greaterOrEqualToken = tokenPrim show updatePositon getToken where
    getToken (GreaterOrEqual p) = Just (GreaterOrEqual p)
    getToken _                  = Nothing

-- - Smaller Token
smallerToken :: ParsecT [Token] st IO(Token)
smallerToken = tokenPrim show updatePositon getToken where
    getToken (Smaller p) = Just (Smaller p)
    getToken _           = Nothing

-- - SmallerOrEqual Token
smallerOrEqualToken :: ParsecT [Token] st IO(Token)
smallerOrEqualToken = tokenPrim show updatePositon getToken where
    getToken (SmallerOrEqual p) = Just (SmallerOrEqual p)
    getToken _                  = Nothing


-- - OpenParentheses Token
openParenthesesToken :: ParsecT [Token] st IO(Token)
openParenthesesToken = tokenPrim show updatePositon getToken where
    getToken (Open_Parentheses p) = Just (Open_Parentheses p)
    getToken _                    = Nothing

-- - CloseParentheses Token
closeParenthesesToken :: ParsecT [Token] st IO(Token)
closeParenthesesToken = tokenPrim show updatePositon getToken where
    getToken (Close_Parentheses p) = Just (Close_Parentheses p)
    getToken _                     = Nothing



-- --------------------------------------------------------
-- Native function tokens
-- --------------------------------------------------------

-- - Print Token
printToken :: ParsecT [Token] st IO(Token)
printToken = tokenPrim show updatePositon getToken where
    getToken (Print p) = Just (Print p)
    getToken _         = Nothing

-- - If Token
ifToken :: ParsecT [Token] st IO(Token)
ifToken = tokenPrim show updatePositon getToken where
    getToken (If p) = Just (If p)
    getToken _      = Nothing

-- - Else Token
elseToken :: ParsecT [Token] st IO(Token)
elseToken = tokenPrim show updatePositon getToken where
    getToken (Else p) = Just (Else p)
    getToken _        = Nothing

-- - Else_If Token
elseIfToken :: ParsecT [Token] st IO(Token)
elseIfToken = tokenPrim show updatePositon getToken where
    getToken (Else_If p) = Just (Else_If p)
    getToken _           = Nothing

-- - End_If Token
endIfToken :: ParsecT [Token] st IO(Token)
endIfToken = tokenPrim show updatePositon getToken where
    getToken (End_If p) = Just (End_If p)
    getToken _          = Nothing

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

-- - Convert int in float
-- Int    Integer value
-- Return Float value
integerToFloat :: Int -> Float
integerToFloat i = do
    let r = fromIntegral(toInteger i) / fromIntegral 1 in r
