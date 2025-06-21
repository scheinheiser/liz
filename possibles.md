Do function type signatures something like this:
```lisp
(dec Int Int Int)
(def div l r (/ l r))
```
Mainly because I like Haskell's top-level type declarations.
Syntax inspired by [par](https://github.com/faiface/par-lang).

Could be like this:
```lisp
(dec Int -> Int -> Int)
(def div l r (/ l r))
```
Which would be a bit more explicit in what's accepted/returned.
But I'm not a fan of the arrows.

---------

Implement my own IR before it's compiled to LLVM/asm.\
I'd like to try and implement my own optimisations (e.g. constant folding) for learning.
