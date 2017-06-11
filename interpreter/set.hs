-- Main
-- Version: 09/06/2017
module Main (main) where

-- External imports
import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
-- Internal imports
import Interpreter
import Lexer

-- - Run
run :: String -> IO()
run "" = error "ERROR"
run command = do
    case unsafePerformIO (parser (getTokens command)) of
    {
        Left err -> print err;
        Right ans -> putStrLn "The program ran successfully!" -- print ans
    }

-- - Main
main :: IO()
main = do
    -- Get the first argument
    (command:args) <- getArgs
    -- Check if there's no argument
    if command == [] then
        putStrLn "ERROR: No arguments."
    else
        -- Check for more than one argument
        if length(args) >= 1 then do
            putStrLn "WARNING: Too many arguments, just the first will be used."
            run command
        else
            run command
