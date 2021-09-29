{-# LANGUAGE LambdaCase #-}

--
-- EPITECH PROJECT, 2021
-- evalExpr [WSL: Ubuntu]
-- File description:
-- Classes
--


module Classes where

import Data.Maybe
import Control.Applicative

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser =
        Parser (\str ->
            case runParser parser str of
                Just (x, y) -> Just (fct x, y)
                _ -> Nothing
        )

instance Applicative Parser where
    pure a = Parser (\x -> Just (a, x))
    (<*>) fct parser =
        Parser (\x ->
            case runParser parser x of
                Just (a, b) -> case runParser fct b of
                    Just (c, d) -> Just (c a, d)
                    _  -> Nothing
                _ -> Nothing
        )

instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) f1 f2 =  Parser (\x ->
            case runParser f1 x of
                Just (a, b) -> Just (a, b)
                _ ->
                    case runParser f2 x of
                        Just (c, d) -> Just (c, d)
                        _ -> empty
        )

instance Monad Parser where
    (>>=) f1 f2 = Parser (\x ->
            case runParser f1 x of
                Nothing -> Nothing
                Just (a, b) -> case runParser (f2 a) b of
                    Nothing -> Nothing
                    Just (c, d) -> Just (c, d)
        )

parseChar :: Char -> Parser Char
parseChar c = Parser (\case
        (x:xs) -> if x == c
            then Just (c, xs)
            else Nothing
        _ -> Nothing
    )

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser (const Nothing)
parseAnyChar (x:xs) = Parser (\str ->
        if isNothing $ runParser (parseChar x) str
            then runParser (parseAnyChar xs) str
            else Just (x, tail str)
    )

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser (\str ->
        case runParser p1 str of
            Nothing -> runParser p2 str
            result -> result
    )

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser (\str ->
        case runParser p1 str of
            Just (a, b) ->
                case runParser p2 b of
                    Just (c, d) -> Just ((a, c), d)
                    _ -> Nothing
            _ -> Nothing
    )

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func p1 p2 = Parser (\str ->
        case runParser (parseAnd p1 p2) str of
            Just ((c1, c2), rest) -> Just (func c1 c2, rest)
            _ -> Nothing
    )

parseMany :: Parser a -> Parser [a]
parseMany p = Parser (\str ->
        case runParser p str of
            Just (c, rest) ->
                case runParser (parseMany p) rest of
                    Just (c2, rest2) -> Just (c : c2, rest2)
                    _ -> Just ([c], rest)
            _ -> Just ([], str)
    )

parseSome :: Parser a -> Parser [a]
parseSome p = Parser (\str ->
        case runParser (parseMany p) str of
            Just ([], _) -> Nothing
            _ -> runParser (parseMany p) str
    )

parseUInt :: Parser Int
parseUInt = Parser (\str ->
        case runParser parseInt str of
            Just (x, xs) -> if x > 0
                then Just (x, xs)
                else Nothing
            _ -> Nothing
    )

parseInt :: Parser Int
parseInt = Parser (\str ->
        case reads str :: [(Int, String)] of
            [(x, xs)] -> Just (x, xs)
            _ -> Nothing
    )

parseTuple :: Parser a -> Parser (a, a)
parseTuple p1 = Parser (\case
        (x:xs) -> if x == '('
            then case runParser p1 xs of
                Just (a,bf:bs) -> if bf == ','
                    then case runParser p1 bs of
                        Just (z, yf:ys) -> if yf == ')'
                            then Just ((a, z), ys)
                            else Nothing
                        _ -> Nothing
                    else Nothing
                _ -> Nothing
            else Nothing
        _ -> Nothing
    )
