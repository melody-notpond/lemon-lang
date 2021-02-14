module Lemon.VM
    ( Chunk (..)
    , VM (..)
    , VMValue (..)
    , newVM
    , execVM
    , builtins
    ) where

import Data.Word
import Data.Bits
import Data.Tuple

data Chunk = Chunk [VMValue] [Word8] deriving (Show)

data VM = VM {
               pc       :: Int
             , chunk    :: Chunk
             , vars     :: [VMValue]
             , stack    :: [VMValue]
         } deriving (Show)

data VMValue = VMInt Integer
                | VMDecimal Float
                | VMCharacter Char
                | NativeFunc (VMValue -> VMValue)
                | LemonFunc Word [VMValue]
                | VMError String

instance Show VMValue where
    show (VMInt i) = show i
    show (VMDecimal f) = show f
    show (VMCharacter c) = show c
    show (NativeFunc _) = "<native>"
    show (LemonFunc _ _) = "<func>"
    show (VMError e) = "error: " ++ e

-- Represents a binary operator on ints and floats
binOp :: (Integer -> Integer -> Integer) -> (Float -> Float -> Float) -> VMValue
binOp iop fop = NativeFunc (\x -> NativeFunc (\y ->
    case x of
        VMInt a ->
            case y of
                VMInt b -> VMInt $ iop a b
                VMDecimal b -> VMDecimal $ fop (fromIntegral a :: Float) b
                _ -> VMError $ "Cannot use operator on " ++ show a ++ " and " ++ show y
        VMDecimal a ->
            case y of
                VMInt b -> VMDecimal $ fop a (fromIntegral b :: Float)
                VMDecimal b -> VMDecimal $ fop a b
                _ -> VMError $ "Cannot use operator on " ++ show a ++ " and " ++ show y
        _ -> VMError $ "Cannot use operator on " ++ show x ++ " and " ++ show y
    ))

-- Represents a binary operator on ints
intOp :: (Integer -> Integer -> Integer) -> VMValue
intOp op = NativeFunc (\x -> NativeFunc (\y ->
    case x of
        VMInt a ->
            case y of
                VMInt b -> VMInt $ op a b
                _ -> VMError $ "Cannot use operator on " ++ show a ++ " and " ++ show y
        _ -> VMError $ "Cannot use operator on " ++ show x ++ " and " ++ show y
    ))

-- Creates a new VM from a chunk
newVM :: Chunk -> VM
newVM chunk = VM 0 chunk [] []

-- Builtin functions
builtins :: [(String, VMValue)]
builtins = [
    ("*", binOp (*) (*)),
    ("/", binOp div (/)),
    ("mod", intOp mod),
    ("+", binOp (+) (+)),
    ("-", binOp (-) (-)),
    ("|", intOp (.|.)),
    ("&", intOp (.&.)),
    ("^", intOp xor)
    ]

{-
 - List of opcodes implemented:
 - - 0x00       - halt
 - - 0x01       - no op
 - - 0x02 a b   - pushes a constant onto the stack (little endian 2 byte argument)
 - - 0x03 a     - pushes a builtin onto the stack
 - - 0x04       - pops a function and an argument from the stack and pushes its return value
 - - 0x05       - pops a value from the stack and binds it to the next variable
 - - 0x06 a b   - pushes the given variable from the top of the variable stack onto the value stack (little endian 2 byte argument)
 - - 0x07 a     - pops `a` variables from the stack of variables
 -
 - List of future implemented opcodes:
 - - bind
 - - eval
 - - load variable
 - - load parameter
 - - jump (for tco/cond)
 - - set parameter (for tco)
 - - jump conditionally (for cond)
 - - load function
 - - returning from functions
 - - more stuff idk
 - -}
-- Executes a VM
execVM :: VM -> VM
execVM vm@(VM pc chunk vars stack) =
    let (Chunk constants bytecode) = chunk in
        if pc >= length bytecode then
            vm
        else case bytecode !! pc of
            -- Halt
            0x00 -> vm

            -- No op
            0x01 -> execVM $ VM (pc + 1) chunk vars stack

            -- Push constant
            0x02 ->
                let c = fromIntegral (bytecode !! (pc + 1)) .|. fromIntegral (bytecode !! (pc + 2)) `shiftL` 8 in
                    execVM $ VM (pc + 3) chunk vars $ (constants !! c) : stack

            -- Push builtin
            0x03 ->
                let c = fromIntegral (bytecode !! (pc + 1)) in
                    execVM $ VM (pc + 2) chunk vars $ snd (builtins !! c) : stack

            -- Apply function
            0x04 ->
                let f = head stack in
                let a = stack !! 1 in
                let stack' = drop 2 stack in
                    case f of
                        NativeFunc g -> execVM $ VM (pc + 1) chunk vars $ g a : stack'
                        LemonFunc v c -> VM pc chunk vars $ VMError "applying lemon functions is not implemented yet" : stack'
                        _ -> VM pc chunk vars $ VMError ("cannot apply " ++ show f) : stack'

            -- Bind variable
            0x05 ->
                let v = head stack in
                let stack' = tail stack in
                    execVM $ VM (pc + 1) chunk (v : vars) stack'

            -- Push variable
            0x06 ->
                let v = fromIntegral (bytecode !! (pc + 1)) .|. fromIntegral (bytecode !! (pc + 2)) `shiftL` 8 in
                    execVM $ VM (pc + 3) chunk vars $ (vars !! v) : stack

            -- Pop scope
            0x07 ->
                let v = fromIntegral (bytecode !! (pc + 1)) in
                    execVM $ VM (pc + 2) chunk (drop v vars) stack

