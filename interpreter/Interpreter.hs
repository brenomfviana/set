-- Interpreter
-- Version: 09/05/2017
-- Author : Breno Viana
module Interpreter where

-- Imports
import System.Environment
import System.IO
import System.IO.Error
import System.Directory
import Data.List
import LexicalAnalyzer

-- Main
main :: IO
main = do
    -- Get arguments
    filesPath <- listOfStringsToFilePath getArgs
    -- Interprets the file
    set filesPath


-- Interpret the file
set :: [FilePath] -> (Bool, [String])
set [] = (False, [])
set (head:tail) =
    -- Check if there is file
    if doesFileExist head
        -- Check file extension
        then if takeExtension head == ".set"
                -- Interprets the file
                then interpret readFile head
                -- Error message
                else putStrLn "ERROR: The file can't be interpreted."
        -- Error message
        else putStrLn "ERROR: File does not exist."


-- Interprets the code
interpret :: String -> (Bool, [String])
interpret [] = (False, [])
interpret code = analyzer(code)


-- Convert
listOfStringsToFilePath :: [String] -> [FilePath]
listOfStringsToFilePath [] = []
listOfStringsToFilePath (head:tail) = decodeString head : listOfStringsToFilePath tail
