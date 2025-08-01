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

Go for a more conventional LISP-style array, using `car` and `cdr`.
Syntax could be something like this:
```lisp
(var mylist #String #("hi" "this is" "my list"))
(assert (== (car mylist) "hi"))
(assert (== (cdr mylist) #("this is" "my list")))
```

---------

With errors, pick up slices of the file where the error is located.
Similar to Haskell errors, it'd help the user see the error more clearly.
