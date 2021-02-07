module Lemon.Parser
    ( ParseResult (..)
    , readExpr
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec

import Lemon.Data

data ParseResult = Ok LemonValue
                 | Err String

parseInt :: Parser LemonValue
parseInt =
    do
        x <- many1 digit
        return $ Int . read $ x

parseDecimal :: Parser LemonValue
parseDecimal =
    do
        whole <- many1 digit
        char '.'
        part <- many digit
        return $ Decimal . read $ whole ++ "." ++ part

parseRawChar :: Parser Char
parseRawChar =
    noneOf "\\\"\'"
        <|> do
            char '\\'
            v <- oneOf "nrta\"\'"
            return $ case v of
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                'a' -> '\a'
                _   -> v

parseChar :: Parser LemonValue
parseChar =
    do
        char '\''
        c <- parseRawChar
        char '\''
        return $ Character c

parseExpr :: Parser LemonValue
parseExpr =  try parseDecimal
         <|> parseInt
         <|> parseChar
         <?> "expected number or character"

readExpr :: String -> ParseResult
readExpr s =
    case parse parseExpr "lemon" s of
        Right v -> Ok v
        Left e -> Err $ show e

{-
 - TODO
 - - strings ("abc" is syntax sugar for (list 'a' 'b' 'c'); parse directly to that)
 - - s expressions
 - - quoted expressions
 - - atoms
 - -}
