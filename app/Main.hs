module Main where

import System.Environment
import System.IO

import Lemon.Data
import Lemon.PreExec
import Lemon.Parser

main :: IO ()
main =
    do
        -- Get file name from arg list
        args <- getArgs
        let filename = head args

        -- Read and parse file
        contents <- readFile filename
        case readExpr contents of
            Ok v -> print $ changeExprBindings v
            Err e -> putStrLn e

