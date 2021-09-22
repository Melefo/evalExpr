--
-- EPITECH PROJECT, 2021
-- evalExpr [WSL: Ubuntu]
-- File description:
-- Errors
--

module Errors where

import Control.Exception ( Exception )
import System.Exit ( ExitCode(ExitFailure), exitWith )

data ParsingException = 
    NoArg | TooManyArg
    deriving Show

instance Exception ParsingException

exit :: IO ()
exit = exitWith $ ExitFailure 84

catchException :: ParsingException -> IO ()
catchException NoArg = putStrLn "You must pass a valid expression to be evaluated" >> exit
catchException TooManyArg = putStrLn "Too many arguments given" >> exit
