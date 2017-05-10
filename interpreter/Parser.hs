-- Parser (Syntax Analyzer)
-- Version: 09/05/2017
-- Author : Breno Viana
module Parser where

-- Imports
import Data.List
import Text.Parsec

-- Tokens
data Token = Integer {line, column :: Int, value :: Integer}
            | Float {line, column :: Int, value :: Float}
instance Show Token where
    show Integer{value=x} = "integer " ++ show x
    show Float  {value=x} = "float " ++ show x


-- Parser
mainparser :: [Token] -> Something
mainparser (tokens) =
