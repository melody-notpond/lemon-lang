module Lemon.Data
    ( LemonValue (..)
    ) where

data LemonValue = Int Integer
                | Decimal Float
                | Character Char
                | Atom String
                | Quote LemonValue
                | SExpr [LemonValue]

instance Show LemonValue where
    show (Int i) = show i
    show (Decimal f) = show f
    show (Atom a) = a
    show (Quote q) = '\'' : show q
    show (SExpr xs) = "(" ++ unwords (map show xs) ++ ")"

