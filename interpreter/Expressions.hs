-- Expression evaluator
-- Version: 09/06/2017
module Expressions where

-- Imports
import Control.Monad.IO.Class
import Text.Parsec
import Lexer
import Parser

-- -----------------------------------------------------------------------------
-- Expression evaluator
-- -----------------------------------------------------------------------------

-- - Expressions
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try  binaryExpression <|> parentExpression <|> unaryExpression

-- - Unary expression
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
unaryExpression :: ParsecT [Token] [(Token,Token)] IO(Token)
unaryExpression = do
                   a <- natToken <|> intToken <|> realToken <|> boolToken
                        <|> textToken
                   return (a)


-- - Boolean Operations
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
booleanOP :: ParsecT [Token] [(Token,Token)] IO(Token)
booleanOP = equalityToken <|> greaterToken <|> greaterOrEqualToken
            <|> smallerToken <|> smallerOrEqualToken


-- - Aritimetc Operations
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
numberOP :: ParsecT [Token] [(Token,Token)] IO(Token)
numberOP = additionToken <|> subtractionToken <|> multiplicationToken


-- - Expression with parentheses
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
parentExpression :: ParsecT [Token] [(Token,Token)] IO(Token)
parentExpression = do
                  a <- open_Parentheses
                  b <- expression <|> parentExpression
                  c <- close_Parentheses
                  return (b)

-- - Binary expression
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
binaryExpression :: ParsecT [Token] [(Token,Token)] IO(Token)
binaryExpression = do
                   a <- natToken <|> intToken <|> realToken <|> parentExpression
                        <|> boolToken <|> textToken
                   b <- numberOP <|> booleanOP
                   c <- expression
                   return (eval a b c)

-- - Evaluation
-- ParsecT          ParsecT
-- [Token]          Token list
-- [(Token, Token)] State
eval :: Token -> Token -> Token -> Token
-- Addition
eval (Nat x p)  (Addition _)       (Nat y _)  = Nat  (x + y) p
eval (Nat x p)  (Addition _)       (Int y _)  = Int  (x + y) p
eval (Int x p)  (Addition _)       (Nat y _)  = Int  (x + y) p
eval (Int x p)  (Addition _)       (Int y _)  = Int  (x + y) p
eval (Real x p) (Addition _)       (Real y _) = Real (x + y) p
-- Subtraction
eval (Nat x p)  (Subtraction _)    (Nat y _)  = Nat  (x - y) p
eval (Int x p)  (Subtraction _)    (Int y _)  = Int  (x - y) p
eval (Real x p) (Subtraction _)    (Real y _) = Real (x - y) p
-- Multiplication
eval (Nat x p)  (Multiplication _) (Nat y _)  = Nat  (x * y) p
eval (Int x p)  (Multiplication _) (Int y _)  = Int  (x * y) p
eval (Real x p) (Multiplication _) (Real y _) = Real (x * y) p
-- Division
-- eval (Nat x p) (Division _) (Nat y _) = Nat (x / y) p
-- eval (Int x p) (Division _) (Int y _) = Int (x / y) p
-- eval (Real x p) (Division _) (Real y _) = Real (x / y) p
-- Equality
eval (Nat x p)  (Equality _)  (Nat y _) = Bool (x == y) p
eval (Int x p)  (Equality _)  (Int y _) = Bool (x == y) p
eval (Real x p) (Equality _) (Real y _) = Bool (x == y) p
eval (Text x p) (Equality _) (Text y _) = Bool (x == y) p
--
eval (Nat x p)  (Greater _)  (Nat y _) = Bool (x > y) p
eval (Int x p)  (Greater _)  (Int y _) = Bool (x > y) p
eval (Real x p) (Greater _) (Real y _) = Bool (x > y) p
--
eval (Nat x p)  (GreaterOrEqual _)  (Nat y _) = Bool (x >= y) p
eval (Int x p)  (GreaterOrEqual _)  (Int y _) = Bool (x >= y) p
eval (Real x p) (GreaterOrEqual _) (Real y _) = Bool (x >= y) p
--
eval (Nat x p)  (Smaller _)  (Nat y _) = Bool (x < y) p
eval (Int x p)  (Smaller _)  (Int y _) = Bool (x < y) p
eval (Real x p) (Smaller _) (Real y _) = Bool (x < y) p
--
eval (Nat x p)  (SmallerOrEqual _)  (Nat y _) = Bool (x <= y) p
eval (Int x p)  (SmallerOrEqual _)  (Int y _) = Bool (x <= y) p
eval (Real x p) (SmallerOrEqual _) (Real y _) = Bool (x <= y) p
