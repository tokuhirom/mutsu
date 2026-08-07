# A role-composed attribute's `is default(...)` does not restore after `= Nil`

Found 2026-08-07 while writing a regression test for ADR-0019 D2c (attribute
`is default(...)` chunk compilation). Unrelated to that change — reproduces
identically on `main` before it.

## Repro

```raku
role R { has $.w is default(21) is rw; }
class Consumer does R { }
my $obj = Consumer.new;
say $obj.w;        # 21 — correct
$obj.w = Nil;
say $obj.w;         # mutsu: "" (Nil) — raku: 21
```

`raku` prints `21` both times. mutsu prints `21` then an empty string (`Nil`)
after the reassignment — the default is used for the initial value but not
for Nil-restore.

## Likely area

A role's `is default(...)` expression is stored in
`Registry::role_attribute_default_exprs` (keyed by `(role, attr)`,
`src/runtime/registration_role_body.rs`) and copied onto the consuming class
into `Registry::class_attribute_default_exprs` (keyed by `(class, attr)`) at
composition time (`src/runtime/registration_class_compose.rs:207-222`). A
*directly declared* class attribute's `is default(...)` instead lives on
`ClassAttributeDef` itself and is evaluated inline by `class_body_has_decl`
(`src/runtime/registration_class_body_attr.rs`) into
`Registry::class_attribute_defaults: HashMap<(String, String), Value>` (an
already-evaluated *value* table, distinct from the *expression* table above
despite the similar name). The Nil-restore path
(`src/runtime/runtime_var_meta.rs:400`) reads `class_attribute_default_exprs`
directly rather than routing through whatever resolves
`class_attribute_defaults` for the direct-declaration case — worth checking
whether the restore path only consults one of the two tables, or evaluates
the expr fresh but with a package/env mismatch (a role's `is default` body
was compiled/stored before the class-composition rename pass runs, so
`self`/`::?CLASS` inside it may not resolve against the consuming class).
Not investigated further — needs a `raku --target=ast` / `MUTSU_TRACE`
comparison to pin down which of the two tables the restore path actually
reads and why it comes up empty for the role-composed case specifically.

## Why deferred

Out of scope for the PR that found it (ADR-0019 D2c-1, which only touches
directly-declared class attributes' `is default`, not role-composed ones —
that's D2c-3, migrating `role_attribute_default_exprs`/
`class_attribute_default_exprs` off raw `Expr`s). Fixing this bug is
independent of that migration; it's a plain restore-path logic gap.
