--
-- EPITECH PROJECT, 2021
-- evalExpr
-- File description:
-- EvalExpr
--

module EvalExpr where

import Control.Exception
import Errors
import Data.Maybe

type Parser a = String -> Maybe (a , String)

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar c (x:xs)
    | c == x = Just(c, xs)
    | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing
parseAnyChar (x:xs) str
    | isNothing $ parseChar x str = parseAnyChar xs str
    | otherwise = Just (x, tail str)

parseOr :: Parser a -> Parser a -> Parser a
parseOr _ _ [] = Nothing
parseOr p1 p2 str
    | isNothing $ p1 str = p2 str
    | otherwise = p1 str

parseAnd :: Parser a -> Parser b -> Parser (a , b)
parseAnd _ _ [] = Nothing
parseAnd p1 p2 str
    | isNothing $ p1 str = Nothing
    | isNothing $ p2 $ tail str = Nothing
    | otherwise =
        case p1 str of
            Just (a, b) -> case p2 b of
                Just (c, d) -> Just ((a,c), d)
                _ -> Nothing
            _ -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith _ _ _ [] = Nothing
parseAndWith func p1 p2 str
    | isNothing $ parseAnd p1 p2 str = Nothing
    | otherwise =
        case parseAnd p1 p2 str of
            Just ((c1, c2), result) -> Just (func c1 c2, result)
            _  -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p str = 
    case p str of
        Just (c, rest) ->
            case parseMany p rest of
                Just (c2, rest2) -> Just (c : c2, rest2)
                _ -> Just ([c], rest)
        _ -> Nothing

evalExpr :: [String] -> IO ()
evalExpr [] = throw NoArg
evalExpr [x] = putStrLn x
evalExpr (_:_) = throw TooManyArg
