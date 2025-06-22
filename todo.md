## Parsing
- [x] Consider adding dedicated syntax for a global variable (like `!!g` or `global`), or disallow it?. (globals would be top level decls)
- [x] Parse basic expressions, like `var`, `set`, binary and unary things.
- [x] Parse nested expressions.
- [x] Basic type inference.
- [x] Parse more complex expressions, like `func` and function calls.
- [x] Change `func` to `def`
- [ ] Add syntax to define custom operators (maybe `(def (<$>) [...] (...)) ?)
- [ ] Add `block` keyword for a block of expressions.

## Semantic Analysis
- [x] Check that there's a `main` func, check that variables outside have the global tag? Up for change, really. (main checking is implemented)
- [x] Variable checking (undeclared, using `undefined`, etc).
- [x] Function checking (returned value matches return type, etc).
- [x] Function calls (arg checking, correct no. of args).
- [x] Type checking.
- [x] Pretty errors.
- [x] Tests for everything
- [ ] Allow declarations in any order.

## Codegen
- [x] Decide between using LLVM or asm for IR. (using QBE instead)
- [ ] Make a QBE pretty printing library.
- [ ] Compile simple expressions (i.e. arithmetic, boolean stuff).
- [ ] Compile complex expressions (i.e. nested stuff, functions).
- [ ] Compile and run the first working liz executable!
