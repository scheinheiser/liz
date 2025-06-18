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
But I'm not a fan of the arrows.
