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
    | c == x = Just (c, xs)
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

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd _ _ [] = Nothing
parseAnd p1 p2 str
    | Just (a, b) <- p1 str
    , Just (c, d) <- p2 b =
        Just ((a, c), d)
    | otherwise = Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith _ _ _ [] = Nothing
parseAndWith func p1 p2 str
    | Just ((c1, c2), rest) <- parseAnd p1 p2 str =
        Just (func c1 c2, rest)
    | otherwise = Nothing

parseMany :: Parser a -> Parser [a]
parseMany p str
    | Just (c, rest) <- p str
    , Just (c2, rest2) <- parseMany p rest =
        Just (c : c2, rest2)
    | Just (c, rest) <- p str
    , Nothing <- parseMany p rest  =
        Just ([c], rest)
    | otherwise = Just([], str)

parseSome :: Parser a -> Parser [a]
parseSome p str
    | Just ([], _) <- parseMany p str = Nothing
    | otherwise = parseMany p str

parseUInt :: Parser Int -- parse an unsigned Int
parseUInt str
    | Just (x, xs) <- parseInt str
    , x > 0 = Just (x, xs)
    | otherwise = Nothing

parseInt :: Parser Int -- parse an signed Int
parseInt str
    | [(x, xs)] <- reads str :: [(Int, String)] =
        Just (x, xs)
    | otherwise = Nothing

parseTuple :: Parser a -> Parser (a, a) -- parse a tuple
parseTuple _ [] = Nothing
parseTuple p (x:xs)
    | x == '('
    , Just (a, y:ys) <- p xs
    , y == ','
    , Just (b, z:zs) <- p ys
    , z == ')' =
        Just ((a, b), zs)
    | otherwise = Nothing

evalExpr :: [String] -> IO ()
evalExpr [] = throw NoArg
evalExpr [x] = putStrLn x
evalExpr (_:_) = throw TooManyArg
