# `proto`/`multi sub` declarations are lexically scoped again

An inner lexical scope that declares its own `proto` was rejected outright:

```raku
proto sub foo($) {*}
multi sub foo(Int $x) { "outer" }
{ proto sub foo($) {*}; multi sub foo(Int $x) { "inner" }; say foo(5); }
say foo(5);
# raku:  inner / outer
# mutsu: Runtime error "Redeclaration of routine 'foo'. Did you mean to declare a multi-sub?"
```

The same shape inside a routine body (`sub bar() { proto sub foo($){*}; multi sub
foo(Int $x){...} }`) failed identically. This is valid Raku refused at declaration
time, so nothing downstream of it could run.

## Root cause

mutsu's routine registry is keyed by the fully-qualified `Package::name`, so an
inner `proto foo` and an outer `proto foo` collide on one key (`GLOBAL::foo`).
Plain single subs already survive this: the block-scope routine-registry
snapshot/restore (`snapshot_routine_registry` / `restore_routine_registry`, taken
around every `BlockScope` and every routine call) puts the outer routine back when
the scope exits, and `register_sub_decl_with_metadata` has an explicit
`allow_lexical_shadow` exemption so the inner declaration is not reported as a
redeclaration on the way in.

`register_proto_decl` never got that exemption. It raised `X::Redeclaration`
unconditionally whenever `functions` or `proto_subs` already held the key — with
no notion of which scope the existing entry belonged to. (ADR-0041 §1.2 described
this defect as "the redeclaration check is name-based and scope-blind for plain
subs"; measured on 2026-09-04, plain subs shadow correctly and it is `proto` that
was scope-blind.)

The proto side needed one more piece than the sub side: a `proto` declared
directly in a *routine body* is not covered by `block_scope_depth`, because a sub
body's declarations are hoisted and registered at a point where the depth counter
is still zero. `Stmt::SubDecl` solves that with the compiler-set `__lexical_hoist`
marker; `Stmt::ProtoDecl` carried no such marker.

## What changed

- `register_proto_decl` takes an `is_lexical_hoist` flag and computes the same
  `allow_lexical_shadow` predicate as `register_sub_decl_with_metadata`
  (`block_scope_depth > 0 || is_lexical_hoist`, minus the EVAL cases). When
  shadowing is allowed, the two redeclaration checks are skipped; the purge of the
  outer name's candidates that already followed them gives the inner proto a fresh
  candidate set, and the snapshot/restore brings the outer one back.
- The compiler marks a body-local `Stmt::ProtoDecl` `__lexical_hoist`, the way
  `hoist_sub_decls` marks a body-local `Stmt::SubDecl`.
- `mark_lexical_body` now tallies protos as well as singles and multis, so a
  *genuine* same-scope redeclaration (two protos of one name, or a proto plus a
  single sub of one name) still raises `X::Redeclaration` exactly as raku does.
  One proto plus any number of `multi`s stays one routine.

Two hoist passes were also emitting registrations that did not identify
themselves as hoist passes, which made a perfectly valid
`{ our proto f($) {*}; our multi f(Int $x) {...} }` die with "Cannot declare
individual multi candidates in 'our' scope" — the hoisted `our multi` registered
before the block's own `our proto` had run. `hoist_nested_our_subs` and the
statement-form bare block's own hoist loop in `Stmt::Block` now add the `__hoisted`
marker that `hoist_sub_decls` has always added, so the check is enforced by the
in-sequence registration (which runs after the proto) instead of by the pre-pass.

## Verified against raku

`t/multi-proto-lexical-scope.t` pins 18 assertions, all measured against Rakudo
v2026.06 first: inner-proto shadowing in a bare block and in a routine body, a
differently-shaped inner proto, an inner `multi` with no inner proto *extending*
the enclosing proto (the common Raku pattern — it must stay a merge), an inner
proto shadowing an outer single sub, both same-scope redeclaration errors,
`our proto`/`our multi` in a nested block, and operator-name candidates merging
across scopes.

An inner `multi` with no inner proto was already correct before this change: raku
merges it into the enclosing proto's candidate set and reports `Ambiguous call` for
two identical signatures, and so did mutsu.
