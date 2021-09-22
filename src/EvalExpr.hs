--
-- EPITECH PROJECT, 2021
-- evalExpr
-- File description:
-- EvalExpr
--

module EvalExpr where

import Control.Exception
import Errors

evalExpr :: [String] -> IO ()
evalExpr [] = throw NoArg
evalExpr [x] = putStrLn "Hello World"
evalExpr (x:_) = throw TooManyArg
