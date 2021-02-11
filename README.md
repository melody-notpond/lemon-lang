# Lemon
Lemon is a simple Lisp whose sole purpose is to be a self hosted language.

## Primatives
List of primatives includes:
- Numbers (ints and floats)
- Characters (`'a'`)
- Numeric operations (`+`, `-`, `*`, `/`, `mod`, `&`, `|`, `^`, `<<`, `>>`)
- `bind`, `bindl`, `bindr`, `func`
- `cond`, `eval`
- `=`, `<`, `>`, `int?`, `float?`, `func?`, `char?`
- `int`, `float`, `char` conversion functions
- `import`, `export`

## Strings
`"abc"` is syntax sugar for `(list 'a' 'b' 'c')`

## Bind and Func
The `bind` functions and the `func` functions do not require parentheses for their last argument. The following is valid:
```
(bind 'true func 'a func 'b a)
(bind 'false func 'a func 'b b)
(bind 'cons func 'a func 'b func 'f f a b)
(bind 'car func 'list list true)
(bind 'cdr func 'list list false)
```

## Bindr and Bindl
The `bindr` and `bindl` functions let you define variable argument functions. Here is an example:
```
(bindr 'list cons)
```

This means that the following two lines are equivalent:
```
(list 1 2 3 4 5 0)
(list 1 (list 2 (list 3 (list 4 (list 5 0)))))
```
