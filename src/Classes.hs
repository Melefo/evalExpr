--
-- EPITECH PROJECT, 2021
-- evalExpr [WSL: Ubuntu]
-- File description:
-- Classes
--

module Classes where

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

parseChar :: Char -> Parser Char
parseChar c = Parser c