--
-- EPITECH PROJECT, 2021
-- evalExpr
-- File description:
-- EvalExpr
--

module EvalExpr where

import Control.Exception
import Errors
import Data.List (intercalate)

removeWhitespace :: String -> String
removeWhitespace x = intercalate "" $ words x

evalExpr :: [String] -> IO ()
evalExpr [] = throw NoArg
evalExpr [x] = putStrLn $ removeWhitespace x
evalExpr (x:_) = throw TooManyArg
