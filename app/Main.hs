module Main where

import Lemon.Data

main :: IO ()
main = print $ SExpr [Atom "bindr", Quote (Atom "list"), SExpr [Atom "func", Quote (Atom "a"), SExpr [Atom "func", Quote (Atom "b"), SExpr [Atom "func", Quote (Atom "f"), SExpr [Atom "f", Atom "a", Atom "b"]]]]]
