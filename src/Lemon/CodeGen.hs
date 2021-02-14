module Lemon.CodeGen
    ( generate
    ) where

import Data.Bits
import Data.List

import Lemon.Data
import Lemon.VM

-- Push literal VMValue onto stack
pushConstant :: VMValue -> Chunk -> Chunk
pushConstant val (Chunk consts bytes) =
    let pos = length consts in
    let low = pos .&. 0xFF in
    let high = (pos `shiftR` 0x8) .&. 0xFF in
    let new_bytes = bytes ++ map fromIntegral [0x02, low, high] in
    let new_consts = consts ++ [val] in
        Chunk new_consts new_bytes

-- Push function call onto stack
pushFunction :: String -> [LemonValue] -> Chunk -> Chunk
pushFunction f args c =
    case findIndex (\(x, _) -> x == f) builtins of
        Just i ->
            -- Builtin function
            let (Chunk consts bytes) = foldr eval c args in
            let new_bytes = bytes ++ [0x03, fromIntegral i] ++ replicate (length args) 0x04 in
                Chunk consts new_bytes
        -- TODO: apply lemon functions

-- Generates bytecode for a single LemonValue
eval :: LemonValue -> Chunk -> Chunk
eval (Int val) = pushConstant $ VMInt val
eval (Decimal val) = pushConstant $ VMDecimal val
eval (Character val) = pushConstant $ VMCharacter val
eval (SExpr (Atom f : args)) = pushFunction f args
-- TODO: evaluate other types

-- Generates bytecode from a list of LemonValues
generate :: [LemonValue] -> Chunk
generate xs = foldr eval (Chunk [] []) $ reverse xs
