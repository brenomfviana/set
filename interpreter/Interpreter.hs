-- Interpreter
-- Version: 10/05/2017
-- Author : Breno Viana
module Interpreter where

-- Imports
import System.Environment
import System.IO
import System.IO.Error
import System.Directory
import System.FilePath.Posix
import Data.List
import Lexer

-- Print messages
checkNumberOfArguments :: [String] -> String
-- Error message
checkNumberOfArguments [] = fail "ERROR: No argument. Enter the file to be compiled."
checkNumberOfArguments (head:tail) =
    if null tail
    -- Success message
    then "Just one argument. OK."
    -- Warning message
    else "WARNING: Too many arguments. Only the first argument will be used."

-- Get first argument
getFirstArgument :: [String] -> String
getFirstArgument (head:tail) = head

-- Interpret the file
set :: String -> Bool
set code =
    --
    putStrLn(getTokens code)
    return True

-- Main
main = do
    args <- getArgs
    putStrLn(checkNumberOfArguments args)
    firstArgument <- getFirstArgument args
    content <- readFile firstArgument
    putStrLn firstArgument
