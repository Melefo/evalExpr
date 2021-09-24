--
-- EPITECH PROJECT, 2021
-- evalExpr
-- File description:
-- EvalExpr
--

module EvalExpr where

import Control.Exception
import Errors
import Data.List
import Data.Maybe

type Parser a = String -> Maybe (a , String)

parseChar :: Char -> Parser Char
parseChar c (x:xs)
    | c == x = Just(c, xs)
    | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing
parseAnyChar (x:xs) str
    | isNothing $ parseChar x str = parseAnyChar xs str
    | otherwise = Just (x, tail str)

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str 
    | isNothing $ p1 str = p2 str
    | otherwise = p1 str

evalExpr :: [String] -> IO ()
evalExpr [] = throw NoArg
evalExpr [x] = putStrLn x
evalExpr (x:_) = throw TooManyArg
