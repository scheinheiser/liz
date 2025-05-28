## Parsing
- [ ] Consider adding dedicated syntax for a global variable (like `!!g` or `global`), or disallow it?.
- [x] Parse basic expressions, like `var`, `set`, binary and unary things.
- [x] Parse nested expressions.
- [x] Basic type inference.
- [x] Parse more complex expressions, like `func` and function calls.

## Semantic Analysis
- [ ] Check that there's a `main` func, check that variables outside have the global tag? Up for change, really.
- [ ] Variable checking (undeclared, using `undefined`, etc).
- [x] Function checking (returned value matches return type, etc).
- [x] Function calls (arg checking, correct no. of args).
- [x] Type checking.
- [ ] Pretty errors.
- [ ] Tests for everything

## Codegen
- [x] Decide between using LLVM or asm for IR. (using LLVM baby)
