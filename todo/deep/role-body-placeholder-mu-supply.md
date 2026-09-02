# A `role` body's placeholder parameters are never supplied their `Mu`

ADR-0048 D7 says a `role` body is signature-capable: raku accepts
`role R { $^c }; class D does R {}` and runs the body at composition with a
value for `$^c`. mutsu's Phase 5 (2026-09-02) landed only the **scope** half of
that decision. The **value** half is open, and this file records exactly where
it is blocked.

## What landed

`placeholder_body_kind` (`src/ast.rs`) classifies `Stmt::RoleDecl` as
`Signature(ArgSupply::AllMu)` instead of `Transparent`. That fixes a real
mis-compilation of the *enclosing* block's signature: `{ role R { $^c } }.arity`
was 1 in mutsu and is 0 in raku, because the role body used to leak its
placeholder outward. It is 0 now. Pinned by `t/placeholder-scope-rejecting.t`.

## What did not land, and why

The `Stmt::RoleDecl` arm in `src/compiler/stmt.rs` still rejects a role body
that uses a placeholder, with `X::Placeholder::Block` — raku accepts it. The
obvious fix (desugar the body to `my $^c = Mu; ...`, the shape the parser's
`rewrite_placeholder_block_modifier_stmt` uses for `{ $a = $^x } unless 0`) was
written and **does not work**, for a structural reason:

A role body is not compiled as one unit. `add_role_decl_plan`
(`src/compiler/decl_plan.rs`) turns it into a list of `DeferredBodyOp`s, and
`run_role_body_for_composition`
(`src/runtime/registration_class_augment.rs:1312`) runs each `Plain` op through
`run_block_raw` — i.e. **each body statement is recompiled as its own
compilation unit at composition time**. So a prepended `my $^c = Mu` statement:

1. compiles as a separate unit, so it does not put `^c` in scope for the later
   statements' own units; and
2. does not stop `compile_unit`'s `is_mainline` check
   (`src/compiler/mod.rs:3516`) firing on the unit that actually reads `$^c` —
   the observed failure was
   `Could not instantiate role 'R' because it died with X::Placeholder::Mainline`.

Making this work needs either a per-op "this is a role body, not the mainline"
flag threaded from `compile_block_raw` down to `compile_unit`, or a role body
that compiles as one chunk rather than per-statement ops. Both touch ADR-0019's
deferred-body machinery, which is why this is `todo/deep` rather than a ticket.

## How much this is worth

Little, on its own. Rakudo's own behaviour here is an implementation artifact,
not a spec: the parameter is left as an **uninitialized `VMNull` register**.
`say $^c` gists as `(Mu)` and `$^c === Mu` is `True`, but `$^c.^name` reports
`VMNull` and `$^c.defined` *throws* inside the composition
(`Could not instantiate role 'R' ... No such method 'defined' for invocant of
type 'VMNull'`). A body with two placeholders gets `VMNull` for both rather than
raku's usual arity failure. Verified 2026-09-02 against raku 2026.06.

So the user-visible defect is narrow: mutsu refuses to compile a file
containing a construct nobody writes, whose raku semantics are garbage. A
corpus scan of `roast/`, `modules/`, `vendor/` and `lib/` for a placeholder used
directly in a role body found **zero** hits. Pick this up when the deferred-body
plumbing is being touched for another reason, not on its own.

## Repro

```
$ raku -e 'role R { say $^c }; class D does R {}; say "done"'
(Mu)
done
$ ./target/debug/mutsu -e 'role R { say $^c }; class D does R {}; say "done"'
Placeholder variable '$^c' may not be used here because the surrounding block does not take a signature.
```
