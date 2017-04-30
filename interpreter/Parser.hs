-- Parser (Syntax Analyzer)
-- Version: 30/04/2017
-- Author : Breno Viana

module Parser where

-- Imports
import Data.List
import Text.Parsec


-- Parser
parser :: [String] -> [String]
-- Check program, declaration of variables,
