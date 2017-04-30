-- Interpreter
-- Version: 30/04/2017
-- Author : Breno Viana

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
    args <- getArgs
    -- Interprets each file
    set args

-- Interpret each file
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
