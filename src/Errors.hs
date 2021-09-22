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
    InvalidInput
    deriving Show

instance Exception ParsingException

usage :: IO ()
usage = putStrLn ""

exit :: IO ()
exit = exitWith $ ExitFailure 84

catchException :: ParsingException -> IO ()
catchException _ = usage >> exit