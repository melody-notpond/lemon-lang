module Main where

import System.Environment
import System.IO

import Lemon.CodeGen
import Lemon.Data
import Lemon.PreExec
import Lemon.Parser
import Lemon.VM

main :: IO ()
main =
    do
        {-
        -- VM
        let chunk = Chunk [VMInt 4, VMDecimal 4.3] [
                -- load 4.3
                0x02, 0x01, 0x00,

                -- bind variable
                0x05,

                -- push variable
                0x06, 0x00, 0x00,

                -- load 4
                0x02, 0x00, 0x00,

                -- load *
                0x03, 0x00,

                -- apply
                0x04,
                0x04--,

                -- pop scope
                --0x07, 0x01
                ]
        print $ execVM $ newVM chunk
        --}
        --{-

        -- Get file name from arg list
        args <- getArgs
        let filename = head args

        -- Read and parse file
        contents <- readFile filename
        case readExpr contents of
            Ok v -> print $ execVM $ newVM $ generate $ changeExprBindings v
            Err e -> putStrLn e
        --}

