# RakuAST: anonymous classes, roles and grammars

`class { }`, `role { }` and `grammar { }` as terms were refused as
`DoStmt(ClassDecl …)` / `DoStmt(RoleDecl …)` in about 80 `t/` files: a
class expression such as `my $c = class { … }`, `class { has $.x }.new` or
`1 but role { … }`.

Measured on rakudo 2026.09, these are the same `Class` / `Role` / `Grammar`
node a declaration has, minus the `name`. The parser wraps such a declaration
in a `DoStmt` and registers it under an internal `__ANON_CLASS_N__` name, so
the converter now unwraps the `DoStmt` and drops that name. The lowering
mints a fresh internal name the way the parser does, through the same
counters, and wraps the declaration back. A class expression composes its
`does` roles through statements at the front of its body, which the
lowering also puts back; the converter leaves them out, since `Trait::Does`
already says it.

The empty name, `class :: is Parent { }`, is another spelling of the same
thing, and rakudo renders it differently: an `anon` scope over the name `::`.
The parser now marks it with an internal `__anon_colons` trait, so the
conversion can tell the two apart and the lowering restores the marker.

Making the files run through the round trip showed three lowering bugs
that had kept a number of other files out of the ratchet, each with its own
checks in `rakuast-lowering-semantics.t`:

- **Chained comparisons.** Rakudo has no chain node: `0 <= $i < 3` is the
  left-nested `ApplyInfix`, chained by the infix's associativity. The
  lowering rebuilt a plain `Binary` of a `Binary`, so `0 <= 9 < 3` computed
  `True < 3`. It now rebuilds the `ChainedCompare`, for the chaining
  operators only (`<=>`, `cmp`, `leg` and the like are structural and never
  chain) and for the negated links (`a !before b before c`). A parenthesized
  left side stays a plain comparison.
- **`-> ::T { … }`.** A pointy block with a single parameter was lowered to a
  `Lambda`, which has no field for the type capture, so `T` was never bound.
  The block keeps its parameter definition when it carries a capture.
- **`$.x = v` as a statement.** It was lowered to `Stmt::Assign`, which has
  no accessor path, so the write was lost. The parser builds an assignment
  expression for it, and the lowering now does too.

Not covered, and unchanged: `class` with traits other than the inheritance
and `does` clauses, `anon class Foo`, an `also is Parent` in the body of the
class (rakudo keeps a `Statement::Also`, mutsu folds it into the traits).
