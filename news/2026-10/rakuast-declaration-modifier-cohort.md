# RakuAST preserves declaration modifiers and named declarator lists

Declarations with `if`, `unless`, `with` and `without` now retain their written
modifier and initializer across the RakuAST frontend. Parsing and lowering share
the expansion that declares variables unconditionally and gates their initializer.
Topical modifiers support list assignment and binding, including array aliases
and named declarator lists. Named declarations retain their signature and use
the same expansion for ordinary source and constructed RakuAST nodes.
Declaring signature bindings yield their Signature value; assignment retains
positional STORE semantics even for named parameters.

Conditional and loop modifier nodes expose their single-expression constructors.
Source records use bounded copies. The compiler also discards intermediate
topicalizer values in statement-position synthetic blocks, so EVAL returns the
unit's final expression rather than a preceding initializer's result.
