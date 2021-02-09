module Lemon.PreExec
    ( changeExprBindings
    ) where

import Control.Monad.ST

import Lemon.Data

data BindMode = BindLeft | BindRight

-- Map bindings over a list of expressions
mapBinding :: [(String, BindMode)] -> [LemonValue] -> [LemonValue]
mapBinding dict [] = []
mapBinding dict (x : xs) = 
    let (dict', v) = changeBindings dict x in
        v : mapBinding dict' xs

-- Convert bindings
changeBindings :: [(String, BindMode)] -> LemonValue -> ([(String, BindMode)], LemonValue)

-- Set bind mode
changeBindings dict (SExpr (Atom "bindl" : Quote (Atom b) : v)) =
    let dict' = (b, BindLeft) : dict in
        (dict', SExpr $ Atom "bindl" : Quote (Atom b) : [snd $ changeBindings dict' $ SExpr v])
changeBindings dict (SExpr (Atom "bindr" : Quote (Atom b) : v)) =
    let dict' = (b, BindRight) : dict in
        (dict', SExpr $ Atom "bindr" : Quote (Atom b) : [snd $ changeBindings dict' $ SExpr v])

-- Regular bindings
changeBindings dict (SExpr (Atom "bind" : Quote (Atom b) : v)) =
    (dict, SExpr $ Atom "bind" : Quote (Atom b) : [snd $ changeBindings dict $ SExpr v])

-- Change functions
changeBindings dict (SExpr (Atom "func" : Quote (Atom a) : v)) =
    (dict, SExpr $ Atom "func" : Quote (Atom a) : [snd $ changeBindings dict $ SExpr v])

-- Check s expression starting with an atom
changeBindings dict (SExpr (Atom a : s)) =
    -- Check if atom is given a binding
    let s' = mapBinding dict s in
        case lookup a dict of
            -- Bind left
            Just BindLeft  -> (dict, SExpr $ bindl a s')

            -- Bind right
            Just BindRight -> (dict, SExpr $ bindr a s')

            -- No binding (check child expressions)
            Nothing        -> (dict, SExpr $ Atom a : s')
            where
                -- Changes left associative bindings
                bindl :: String -> [LemonValue] -> [LemonValue]
                bindl a (x : y : z : xs) = Atom a : SExpr (bindl a $ x : y : init (z : xs)) : [last $ z : xs]
                bindl a [x, y] = [Atom a, x, y]
                bindl a xs = xs

                -- Changes right associative bindings
                bindr :: String -> [LemonValue] -> [LemonValue]
                bindr a (x : y : z : xs) = Atom a : x : [SExpr $ bindr a (y : z : xs)]
                bindr a [x, y] = [Atom a, x, y]
                bindr a xs = xs

-- Check s expression not starting with an atom
changeBindings dict (SExpr s) = (dict, SExpr $ mapBinding dict s)

-- Check quoted expressions
changeBindings dict (Quote s) = (dict, Quote $ snd $ changeBindings dict s)

-- Everything else doesn't change
changeBindings dict v = (dict, v)

-- Check with default arguments
changeExprBindings :: LemonValue -> LemonValue
changeExprBindings v =
    snd $ changeBindings [
        ("+", BindLeft),
        ("-", BindLeft),
        ("*", BindLeft),
        ("/", BindLeft),
        ("div", BindLeft),
        ("mod", BindLeft),
        ("&", BindLeft),
        ("|", BindLeft),
        ("^", BindLeft),
        (">>", BindLeft),
        ("<<", BindLeft)
    ] v
