# `our Int ($a, $b)` and `our Int $.x` are refused too

```raku
our Int ($a, $b);          # raku: ===SORRY!===   mutsu: compiled
class C { our Int $.x }    # raku: ===SORRY!===   mutsu: compiled
```

`news/2026-09/state-typed-declarations-hoist-and-our-typed-declarations-are-refused.md`
made `our TYPE $x` / `@a` / `%h` / `\x` fail to compile as rakudo does. These
two spellings escaped it, for the same reason in both cases: the AST does not
carry the `our` down to the node the constraint is attached to.

- **Destructuring.** `our Int ($a, $b)` lowers to a `SyntheticBlock` of
  `Stmt::VarDecl`s, each carrying the constraint but with `is_our: false`, so
  the compiler's check could not see it.
- **Attribute.** `our Int $.x` is a `Stmt::HasDecl`, which the class-body
  planner compiles — it never reaches `compile_stmt`'s `VarDecl` arm at all.

## The fix

Both are refused in the **parser**, sharing one
`our_type_constraint_error` helper that mints the same `X::Comp::AdHoc`
message the compiler's arm does. That is where rakudo refuses them too, and it
avoids the two awkward alternatives the ticket weighed: propagating `is_our`
onto every produced `VarDecl` (changing what every consumer of those nodes
sees) and raising a statement-shaped refusal out of the class-body planner,
whose failure mode at that point is registration-time.

The scalar/array/hash/sigilless forms keep their compiler-side refusal, which
is deliberate: rakudo still *parses* `our Int $x` — `Q[our Int $x].AST` builds a
`RakuAST::VarDeclaration::Simple` carrying both `scope => "our"` and its
`type`, pinned by `t/rakuast-vardecl-scoped.t` — and refuses only to compile it.

Everything around them still compiles, all measured against raku: the untyped
`our $.x` (so the test is on the *constraint*, not on `our` plus attribute),
`has Int $.x`, `my Int $.x`, `my Int ($a, $b)`, `state Int ($c, $d)`,
`our ($a, $b)`, `our constant K` and `our Int constant J`.

`t/state-and-our-typed-declarations.t` grows six rows for this (24 assertions
now, all passing under rakudo).
