# melisma
## Overview
**melisma** is a tiny interpreter for ML-like language, written in Haskell. It supports library, type inference, lazy evaluation, and let polymorphism.

## How to use
It is strongly recommended to create a sandbox before installing **melisma**. The following commands will create a sandbox, install, and run **melisma**.
```
cabal sandbox init
cabal configure
cabal install
cabal run
```
## sample
```
koba-e964@ubuntu:~/srcview/melisma$ .cabal-sandbox/bin/melisma 
id : forall 'a. 'a -> 'a
not : bool -> bool
const : forall 'a 'b. 'a -> 'b -> 'a
flip : forall 'a 'b 'c. ('b -> 'a -> 'c) -> 'a -> 'b -> 'c
compose : forall 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
fst : forall 'a 'b. ('a, 'b) -> 'a
snd : forall 'a 'b. ('a, 'b) -> 'b
map : forall 'a 'b. ('b -> 'a) -> ['b] -> ['a]
foldl : forall 'a 'b. ('b -> 'a -> 'b) -> 'b -> ['a] -> 'b
foldr : forall 'a 'b. ('b -> 'a -> 'a) -> 'a -> ['b] -> 'a
foldl_s : forall 'a 'b. ('b -> 'a -> 'b) -> 'b -> ['a] -> 'b
head : forall 'a. ['a] -> 'a
tail : forall 'a. ['a] -> ['a]
last : forall 'a. ['a] -> 'a
append : forall 'a. ['a] -> ['a] -> ['a]
concat : forall 'a. [['a]] -> ['a]
reverse : forall 'a. ['a] -> ['a]
null : forall 'a. ['a] -> bool
length : forall 'a. ['a] -> int
intersperse : forall 'a. 'a -> ['a] -> ['a]
and_all : [bool] -> bool
filter : forall 'a. ('a -> bool) -> ['a] -> ['a]
all : forall 'a. ('a -> bool) -> ['a] -> bool
iterate : forall 'a. ('a -> 'a) -> 'a -> ['a]
repeat : forall 'a. 'a -> ['a]
replicate : forall 'a. int -> 'a -> ['a]
take : forall 'a. int -> ['a] -> ['a]
drop : forall 'a. int -> ['a] -> ['a]
split_at : forall 'a. int -> ['a] -> (['a], ['a])
nth : forall 'a. int -> ['a] -> 'a
take_while : forall 'a. ('a -> bool) -> ['a] -> ['a]
drop_while : forall 'a. ('a -> bool) -> ['a] -> ['a]
elem : forall 'a 'b. ('a -> 'b -> bool) -> 'a -> ['b] -> bool
zip : forall 'a 'b. ['a] -> ['b] -> [('a, 'b)]
zip_with : forall 'a 'b 'c. ('b -> 'c -> 'a) -> ['b] -> ['c] -> ['a]
enum_from_to : int -> int -> [int]
sum : [int] -> int
sum_s : [int] -> int
times : forall 'a. int -> ('a -> 'a) -> 'a -> 'a
times_s : forall 'a. int -> ('a -> 'a) -> 'a -> 'a
bit_count : int -> int
> head [];;
- : forall 'a. 'a
error: EvalError "Matching not exhaustive"
> last [];;
- : forall 'a. 'a
error: EvalError "Matching not exhaustive"
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
> false || head [];;
- : bool
error: EvalError "Matching not exhaustive"
> melisma: <stdin>: hGetLine: end of file
```
(In the last line, input is EOF.)
## Dependency
* ghc
* happy
* alex
* base >=4 && <5
* mtl >=2.2
* containers >=0.5
* array >=0.4
* HUnit >=1.2
* transformers >=0.3
* primitive >=0.5

## Others
**melisma** is one of many implementations of ML-like languages. Any pull requests, bug reports, and comments are highly appreciated.

