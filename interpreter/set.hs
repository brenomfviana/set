-- Interpreter
-- Version: 09/06/2017
module Main (main) where

-- Imports
import Control.Monad.IO.Class
import System.IO.Unsafe
import Interpreter
import Lexer

-- Main
main :: IO ()
main = do
    case unsafePerformIO (parser (getTokens "TestFiles/test-i.set")) of
    {
        Left err -> print err;
        Right ans -> print "The program ran successfully!" -- ans
    }
