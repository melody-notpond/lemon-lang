module Main where

import System.Environment
import System.IO

import Lemon.Data
import Lemon.Parser

main :: IO ()
main =
    do
        args <- getArgs
        let filename = head args
        contents <- readFile filename
        case readExpr contents of
            Ok v -> print v
            Err e -> putStrLn e

