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
import Data.Char

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
                | VMFunc Int [VMValue]
                | VMError String

instance Show VMValue where
    show (VMInt i) = show i
    show (VMDecimal f) = show f
    show (VMCharacter c) = show c
    show (NativeFunc _) = "<native>"
    show (VMFunc _ _) = "<func>"
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

-- Represents a comparison operator on numbers
compOp :: (Integer -> Integer -> Bool) -> (Float -> Float -> Bool) -> VMValue
compOp iop fop = NativeFunc (\x -> NativeFunc (\y ->
    case x of
        VMInt a ->
            case y of
                VMInt b -> VMInt $ if iop a b then 1 else 0
                VMDecimal b -> VMInt $ if fop (fromIntegral a :: Float) b then 1 else 0
                _ -> VMError $ "Cannot use operator on " ++ show a ++ " and " ++ show y
        VMDecimal a ->
            case y of
                VMInt b -> VMInt $ if fop a (fromIntegral b :: Float) then 1 else 0
                VMDecimal b -> VMInt $ if fop a b then 1 else 0
                _ -> VMError $ "Cannot use operator on " ++ show a ++ " and " ++ show y
        _ -> VMError $ "Cannot use operator on " ++ show x ++ " and " ++ show y
    ))


-- Creates a new VM from a chunk
newVM :: Chunk -> VM
newVM chunk = VM 0 chunk [] []

-- Builtin functions
builtins :: [(String, VMValue)]
builtins = [
    -- Binary operators on numbers
    ("*", binOp (*) (*)),
    ("/", binOp div (/)),
    ("mod", intOp mod),
    ("+", binOp (+) (+)),
    ("-", binOp (-) (-)),
    ("|", intOp (.|.)),
    ("&", intOp (.&.)),
    ("^", intOp xor),

    -- Comparison operators
    ("==", compOp (==) (==)),
    ("!=", compOp (/=) (/=)),
    ("<=", compOp (<=) (<=)),
    (">=", compOp (>=) (>=)),
    ("<", compOp (<) (<)),
    (">", compOp (>) (>)),

    -- Type checking operators
    ("int?",   NativeFunc (\case { VMInt _ -> VMInt 1; _ -> VMInt 0 })),
    ("float?", NativeFunc (\case { VMDecimal _ -> VMInt 1; _ -> VMInt 0 })),
    ("char?",  NativeFunc (\case { VMCharacter _ -> VMInt 1; _ -> VMInt 0 })),
    ("func?",  NativeFunc (\case { VMFunc _ _ -> VMInt 1; NativeFunc _ -> VMInt 1; _ -> VMInt 0 })),

    -- Conversion functions
    ("int", NativeFunc (\case { VMInt i -> VMInt i; VMDecimal f -> VMInt $ floor f; VMCharacter c -> VMInt $ fromIntegral $ ord c })),
    ("float", NativeFunc (\case { VMInt i -> VMDecimal $ fromIntegral i; VMDecimal f -> VMDecimal f; VMCharacter c -> VMDecimal $ fromIntegral $ ord c })),
    ("char", NativeFunc (\case { VMInt i -> VMCharacter $ chr $ fromIntegral i; VMCharacter c -> VMCharacter c; VMDecimal f -> VMCharacter $ chr $ fromIntegral $ floor f }))
    ]

{-
 - List of opcodes implemented:
 - - 0x00           - halt
 - - 0x01           - no op
 - - 0x02 a b       - pushes a constant onto the stack (little endian 2 byte argument)
 - - 0x03 a         - pushes a builtin onto the stack
 - - 0x04           - pops a function and an argument from the stack and pushes its return value
 - - 0x05           - pops a value from the stack and binds it to the next variable
 - - 0x06 a b       - pushes the given variable from the top of the variable stack onto the value stack (little endian 2 byte argument)
 - - 0x07 a         - pops `a` variables from the stack of variables
 - - 0x08 a b       - pops a value from the stack and replaces a variable with that value
 - - 0x09 a b c d   - pushes a function with no closed values and the given call address (little endian 4 byte argument)
 - - 0x0A a b       - closes over the given variable
 - - 0x0B           - returns from a function (pops a value and return address and repushes the return value)
 -
 - List of opcodes to implement:
 - - 0x0C a b       - jumps relative to the current position (signed little endian 2 byte argument)
 - - 0x0D a b       - pops a value from the stack and jumps relative to the current position if the popped value is truthy (signed little endian 2 byte argument)
 - - 0x0E a b       - pops a value from the stack and jumps relative to the current position if the popped value is falsy (signed little endian 2 byte argument)
 -
 - List of future implemented opcodes:
 - - eval
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
                        VMFunc addr closed -> execVM $ VM addr chunk (closed ++ vars) $ VMInt (fromIntegral pc + 1) : stack'
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

            -- Reassign variable
            0x08 ->
                let n = fromIntegral (bytecode !! (pc + 1)) .|. fromIntegral (bytecode !! (pc + 2)) `shiftL` 8 in
                let v = head stack in
                let stack' = tail stack in
                    execVM $ VM (pc + 3) chunk (take (n - 1) vars ++ [v] ++ drop n vars) stack'

            -- New function
            0x09 ->
                let addr = fromIntegral (bytecode !! (pc + 1)) .|. fromIntegral (bytecode !! (pc + 2)) `shiftL` 8 .|. fromIntegral (bytecode !! (pc + 3)) `shiftL` 16 .|. fromIntegral (bytecode !! (pc + 4)) `shiftL` 24 in
                    execVM $ VM (pc + 5) chunk vars $ VMFunc addr [] : stack

            -- Close variables
            0x0A ->
                let v = fromIntegral (bytecode !! (pc + 1)) .|. fromIntegral (bytecode !! (pc + 2)) `shiftL` 8 in
                let f = head stack in
                let stack' = tail stack in
                    case f of
                        VMFunc addr closed -> execVM $ VM (pc + 3) chunk vars $ VMFunc addr ((vars !! v) : closed) : stack'
                        _ -> VM pc chunk vars $ VMError ("cannot close over a variable for " ++ show f) : stack'

            -- Returning
            0x0B ->
                let v = head stack in
                let addr = case stack !! 1 of VMInt i -> i in
                let stack' = drop 2 stack in
                    execVM $ VM (fromIntegral addr) chunk vars $ v : stack'

