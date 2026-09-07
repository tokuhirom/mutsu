# `our Int ($a, $b)` and `our Int $.x` are still accepted

Left over from
`news/2026-09/state-typed-declarations-hoist-and-our-typed-declarations-are-refused.md`,
which made `our TYPE $x` / `@a` / `%h` / `\x` fail to compile as rakudo does.
Two spellings escaped that check, for the same reason in both cases: the AST
does not carry the `our` down to the node the constraint is attached to.

## Repro

```raku
our Int ($a, $b);          # raku: ===SORRY!===   mutsu: compiles
class C { our Int $.x }    # raku: ===SORRY!===   mutsu: compiles
```

rakudo's message for both is the same one the scalar form already produces:
`Cannot put a type constraint on an 'our'-scoped variable`. (`class C { our
$.x }` — untyped — is legal in both, so the check must key on the constraint,
not on `our`+attribute.)

## Root cause

- **Destructuring.** `our Int ($a, $b)` lowers to a `SyntheticBlock` of two
  `Stmt::VarDecl`s, each with `type_constraint: Some("Int")` but
  `is_our: false` — `parse_destructuring_decl` takes `is_our` as an argument and
  does not put it on the produced declarations. So the compiler's check
  (`src/compiler/stmt.rs`, the `Stmt::VarDecl` arm) cannot see it.
- **Attribute.** `our Int $.x` is a `Stmt::HasDecl { is_our: true,
  type_constraint: Some("Int"), … }`, which is not compiled through
  `compile_stmt` at all — class attributes go through the class-body planner
  (`src/compiler/decl_plan.rs`, `compile_class_attr_decl`).

## Why it is a ticket

The destructuring half is either a parser fix (propagate `is_our` onto each
produced `VarDecl`, which changes what every consumer of those nodes sees) or a
check in the destructuring parser itself. The attribute half needs the refusal
raised from the class-body planner, whose failure mode at that point is
registration-time rather than statement-time — a different shape from the
`LoadConst` + `Die` the `VarDecl` arm emits, and worth doing deliberately.

Neither is a silent wrong answer: both simply accept a declaration rakudo
rejects.

## Acceptance

Both repros are refused with rakudo's message; `class C { our $.x }`,
`class C { has Int $.x }`, `class C { my Int $.x }` and `my Int ($a, $b)` all
keep compiling; `t/state-and-our-typed-declarations.t` grows the two rows and
stays green.
