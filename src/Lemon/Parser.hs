module Lemon.Parser
    ( ParseResult (..)
    , readExpr
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec

import Lemon.Data

-- Represents the result of parsing
data ParseResult = Ok [LemonValue]
                 | Err String

-- Parses an integer
parseInt :: Parser LemonValue
parseInt =
    do
        -- An integer is many digits
        x <- many1 digit
        return $ Int . read $ x

-- Parses a float
parseDecimal :: Parser LemonValue
parseDecimal =
    do
        -- A float is many digits, followed by a dot, followed by more digits
        whole <- many1 digit
        char '.'
        part <- many digit
        return $ Decimal . read $ whole ++ "." ++ part

-- Parses an escaped character (useful for both parseChar and parseString)
parseRawChar :: Parser Char
parseRawChar =
    -- A character is not a backslash or quote
    noneOf "\\\"\'"
        <|> do
            -- Or an escaped character
            char '\\'
            v <- oneOf "nrta\"\'"
            return $ case v of
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                'a' -> '\a'
                _   -> v

-- Parses a character
parseChar :: Parser LemonValue
parseChar =
    do
        -- A character is a single quote, followed by a character value, followed by a single quote
        char '\''
        c <- parseRawChar
        char '\''
        return $ Character c

-- Parses a string
parseString :: Parser LemonValue
parseString =
    do
        -- A string is a double quote, followed by a bunch of characters, followed by a double quote
        char '\"'
        str <- many parseRawChar
        char '\"'
        -- "abc" returns `SExpr [Atom "list", Character 'a', Character 'b', Character 'c']`
        return $ SExpr $ Atom "list" : map Character str

-- Parses a list
parseList :: Parser LemonValue
parseList =
    do
        -- Parentheses surrounding space-separated expressions
        char '('
        spaces
        list <- many $ try parseExpr
        spaces
        char ')'
        return $ SExpr list

-- Parses a quote
parseQuote :: Parser LemonValue
parseQuote =
    do
        -- A quote is some expression prefixed with a single quote
        char '\''
        Quote <$> parseExpr

-- Parses an atom
parseAtom :: Parser LemonValue
parseAtom =
    do
        -- An atom is a string of any of these characters
        atom <- many1 $ oneOf "!@$%^&*_-+=.,<~>/?qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCM1234567890"
        return $ Atom atom

-- Parses an expression
parseExpr :: Parser LemonValue
parseExpr =  spaces >> (
             try parseDecimal
         <|> parseInt
         <|> try parseChar -- if fails, match against parseQuote
         <|> parseQuote
         <|> parseString
         <|> parseList
         <|> parseAtom
         <?> "expected number, character, string, atom, quoted expression, or s expression"
         )

-- Reads an expression and parses it
readExpr :: String -> ParseResult
readExpr s =
    case parse (
        do
            spaces
            v <- many $ try parseExpr
            spaces
            eof
            return v
    ) "lemon" s of
        Right v -> Ok v
        Left e -> Err $ show e

