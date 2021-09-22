--
-- EPITECH PROJECT, 2021
-- evalExpr
-- File description:
-- Main
--

module Main where

import System.Environment ( getArgs )
import Control.Exception ( handle )
import EvalExpr (evalExpr)
import Errors ( catchException )

main :: IO ()
main = handle catchException $
    getArgs >>= evalExpr
