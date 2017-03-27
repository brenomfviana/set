--
-- Version: 25/03/2017
-- Author : Breno Viana

-- Imports
import System.Environment
import System.IO
import System.IO.Error
import System.Directory
import Data.List
import LexicalAnalyzer

--
main:: IO
main = do
    -- Get arguments
    args <- getArgs
    -- Interprets each file
    interpret args

-- Interpret each file
interpret:: [FilePath] -> (Bool, [String])
interpret [] = False
interpret (head:tail) =
    -- Check if there is file
    if doesFileExist head
        -- Check file extension
        then if takeExtension head == ".set"
                -- Interprest the file
                then set readFile head
                -- Error message
                else putStrLn "ERROR: The file can't be interpreted."
        -- Error message
        else putStrLn "ERROR: File does not exist."

-- Interprets the code
set:: String -> (Bool, [String])
set code = analyzer(code)
