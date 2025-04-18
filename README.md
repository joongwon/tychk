# tychk

**A Non-Standard Hindley-Milner Type Checker in Compositional Style with Free Variable Handling**

This repository contains a unique type checking algorithm for the Hindley-Milner type system. Unlike standard implementations, `tychk` is designed to infer the types of expressions containing free variables. Also, it is implemented in a compositional style, meaning that the type of a larger expression is derived from the types of its sub-expressions, even if those sub-expressions have free variables.

* **Type Checking Algorithm:** The central type checking function `check` is implemented in `lib/type.ml`.
* **Algorithm Signature:** The `check` function has the following type:
    ```ocaml
    Expr.t -> (ctx * t) option
    ```
    This indicates that given an expression of type `Expr.t`, the `check` function attempts to infer its type and the resulting type context. It returns an optional tuple of the inferred context (`ctx`) and the inferred type (`t`). If type checking fails, it returns `None`.

## Repository Structure

```
tychk/
├── lib/
│   ├── expr.ml     # Definition of the expression type 't'
│   └── type.ml     # Definition of types 't', contexts 'ctx', and the 'check' algorithm
├── bin/
│   └── main.ml     # Main entry point for the type checker; feel free to try your own expression here
└── README.md
```

## Getting Started

To use or explore `tychk`, you will need an OCaml development environment.

```bash
git clone <repository_url>
cd tychk
opam switch create .
eval $(opam env)
opam install . --deps-only
dune exec tychk
```
