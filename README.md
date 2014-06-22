# kobaml
## Overview
**kobaml** is a tiny interpreter for ML-like language, written in Haskell. It supports library, type inference, lazy evaluation, and let polymorphism.

## How to use
`make` will create the executable file `mcalc`. `mcalc` loads the library file(`stdlib.txt`) on startup.
## sample
```
koba-e964@debian-koba-e964-0:~/srcview/kobaml$ rlwrap ./mcalc 
id : forall 'a. 'a -> 'a
not : bool -> bool
const : forall 'a 'b. 'a -> 'b -> 'a
flip : forall 'a 'b 'c. ('b -> 'a -> 'c) -> 'a -> 'b -> 'c
compose : forall 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
fst : forall 'a 'b. ('a, 'b) -> 'a
snd : forall 'a 'b. ('a, 'b) -> 'b
... (Omitted)
times_s : forall 'a. int -> ('a -> 'a) -> 'a -> 'a
> null[head[]];;
- : bool
 = False
> head [];;
- : forall 'a. 'a
error: EvalError "Matching not exhaustive"
> last [];;
- : forall 'a. 'a
error: EvalError "Matching not exhaustive"
> [1..10] ++ [head []];;
parseError: [PLUS,LBRACKET,ID "head",LBRACKET,RBRACKET,RBRACKET,EOC]
> compose [1..10] [head []];;
error: TypeError "Cannot unify 't0 -> 't1 with ['t9] (T_T)"
> concat [1..10] [head []];;
error: TypeError "Cannot unify ['t0] with ['t7] -> 't8 (T_T)"
> append [1..10] [head []];;
- : [int]
 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...]
> true || head [];;
- : bool
 = True
> mcalc: <stdin>: hGetLine: end of file
```
(In the last line, input is EOF.)
## Dependency
* ghc
* happy
* alex
* mtl-2.2.1

## Others
**kobaml** is one of many implementations of ML-like languages. Any pull requests, bug reports, and comments are highly appreciated.

