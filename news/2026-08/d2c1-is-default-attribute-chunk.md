# ADR-0019 D2c-1: attribute `is default(...)` compiles to a child chunk

A scoping pass over D2c ("compile defaults/constraints as child chunks",
`todo/deep/adr0019-d2c-attribute-default-chunks.md`) found the box
substantially bigger than the ADR text implies: at least 15 eval sites
across 5 env-setup shapes, plus an architectural gap D2b didn't close
(`CompiledAttrDecl::from_stmt` has no `Compiler` in scope at 3 of its 4 call
sites). This lands the first, narrowly-scoped slice of the recommended
D2c-1/2/3 split: the `is default(...)` trait argument on a directly-declared
class attribute.

`CompiledAttrDecl::is_default` is now a `DeclTraitArg` (the existing
`Literal`/`Compiled`/`Ast` enum used elsewhere for declaration trait
arguments) instead of a raw `Expr`. `Compiler::add_class_decl_plan`
precompiles every own attribute's `is default(...)` argument into a
name-keyed `Vec<(Symbol, DeclTraitArg)>`
(`CompiledClassDeclPlan::is_default_chunks`), threaded through
`ClassDeclModifiers` → `run_class_body` → `ClassBodyCx` to
`class_body_has_decl`, which looks its current attribute's chunk up **by
name** rather than by registration-walk position. Keying by name instead of
position was a deliberate simplification over the position-zip approach
the scoping pass suggested: it sidesteps entirely the risk of the
registration-time traversal order silently drifting from the compile-time
one (SyntheticBlock flattening, nested-sub-declared attributes), at the cost
of a linear per-attribute scan — fine at ordinary attribute counts.

`is_default` was picked over `default`/`where_constraint` for this first
slice because it is read-and-discarded once at registration time
(`class_body_has_decl` evaluates it immediately) rather than stored on
`ClassAttributeDef` for later construction-time evaluation. Migrating
`default`/`where_constraint` requires `ClassAttributeDef` itself to change
in lockstep — that stays D2c-2. Only 2 of the 4 `CompiledAttrDecl::from_stmt`
call sites ever read `.is_default` at all (`class_body_has_decl`,
`role_body_has_decl`); the mainline/EVAL `has`-outside-class error path and
`augment class` never did, so they needed no behavior change beyond passing
`None` for the new precompiled-chunk parameter. `role_body_has_decl` still
stashes a raw `Expr` into the `role_attribute_default_exprs` registry table
(D2c-3's territory — role attribute defaults defer to composition time) via
a new `DeclTraitArg::as_expr()` escape valve, since no compiled plan exists
for that path yet.

While writing the regression test for this slice, a pre-existing, unrelated
bug surfaced: a role-composed attribute's `is default(...)` does not restore
correctly after `= Nil` (`todo/tickets/role-attribute-is-default-nil-restore-after-composition.md`).
Reproduces identically before this change; left for separate investigation.
