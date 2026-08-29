# A selective import's captured proto/multi now survives its import scope popping

`roast/S32-list/skip.t` deliberately imports `Test` selectively — binding
`plan`/`subtest`/`is`/`is-deeply`/`throws-like` as `&`-sigil VALUES out of a
`do` block — precisely so the core `skip` routine keeps its own meaning
(`Test` also exports a `skip` sub that would otherwise shadow it):

```raku
BEGIN my (&plan, &subtest, &is, &is-deeply, &throws-like) = do {
    use Test;
    (&plan, &subtest, &is, &is-deeply, &throws-like)
}
```

Under `MUTSU_REAL_TEST=1` (the vendored upstream `Test.rakumod`, not mutsu's
native TAP provider — see `todo/deep/vendor-real-test-module.md`) this died
with `Unknown function: plan` the moment `plan` was called, even though
`Test::plan` was still perfectly declared. The ledger's own row for the file
described it as "routine-value self-recursion after an import scope pops" —
accurate as far as it went, but that recursion turned out to be a downstream
symptom of two separate, more fundamental bugs, both in how mutsu handles
capturing a `proto`/`multi` routine as a first-class `&`-sigil value from
inside an import that is lexically scoped to a block.

## Bug A: a lazy by-name reference outlives the alias it depends on

`resolve_code_var` (`src/runtime/accessors_resolve.rs`), used to evaluate
`&plan`, had two branches for a name with concrete multi-candidate bodies: a
name with NO explicit `proto sub` built a `Sub` that captured every candidate
BY VALUE, so the callable kept working forever; a name WITH an explicit proto
instead built a lazy `Value::routine_parts(current_package, name)` reference
that re-resolves the bareword by name at *call* time.

That lazy reference is fine for an ordinarily-declared proto — its package
registration never goes away — but `has_proto` found `plan` here by searching
`bare_name_packages()`, which for a mainline call is `GLOBAL`: the
*importing* package's alias (`GLOBAL::plan`), not the package (`Test`) that
actually declared the routine. An import is lexically scoped to the block
that asked for it, so `pop_import_scope` correctly removes `GLOBAL::plan`
once the `do {}` block ends — after which the captured reference pointed at
nothing, and the later `plan 1` call died.

The fix unifies the two branches: whenever concrete multi-candidate bodies
exist — whether or not the name also has an explicit proto — capture them BY
VALUE at the point `&name` is evaluated, the same way the non-proto branch
already did. The resulting `Sub` keeps working regardless of what the
registry does to the name afterward, matching Raku's real semantics: a
captured `&code` value stays bound to the actual routine, independent of
whether the short name used to capture it is still lexically visible.

## Bug B: a nested `use` inside a top-level BEGIN was invisible to the hoister

Separately, `run_toplevel_begin_phasers` (`src/runtime/run_prelude.rs`)
pre-runs a "hoistable" top-level `BEGIN` at compile time through a
`eval_block_value` sub-interpreter, so a later mainline read sees its side
effects. `begin_body_is_hoistable` gates this: a body containing a
declaration (including `use`), a bareword, or a call is excluded. The `use`
check only looked at the BEGIN's own top-level statements, though — a `use`
nested inside a `do {}` block slipped past it. `eval_block_value`'s merge-back
into the shared `env` deliberately does not persist `&`-callable writes (only
plain lexicals), so a hoisted `BEGIN my &plan = do { use Test; &plan }`
(single-variable binding) lost its captured value entirely before the
mainline ever ran.

A list-destructured binding (`BEGIN my (&plan, &is) = do {...}`, the exact
shape `S32-list/skip.t` uses) happened to dodge this by accident — its
desugared AST contains the substring `Call` in Debug form, which already
disqualified hoisting for an unrelated reason — so this bug stayed masked in
that file and only surfaced once Bug A was isolated with a single-variable
reduction. The fix extends the same whole-tree Debug-string search the
`Call`/`BareWord` checks already use to also catch `Use { ` anywhere in the
body, not just at the top level.

## Fallout: `.arity`/`.count` on a materialized dispatcher

Bug A's fix widened an existing "materialize a multi-dispatcher `Sub`" code
path (previously used only for a multi with no explicit proto) to also cover
the proto'd case. That path's `Sub` carries no `param_defs` of its own — the
real signature information lives in its captured
`__mutsu_multi_dispatch_candidates`/`__mutsu_multi_dispatch_name` env keys,
which the `.candidates`/`.signature`/`.cando`/`.wrap` methods already knew to
consult. `.arity`/`.count` did not, so `make test` caught a real regression:
`t/signature-arity-count.t`'s `my proto sub a($, $?) {*}` capture started
answering `0`/`0` for `&a.arity`/`&a.count` instead of `1`/`2`. Fixed by
teaching the same two handlers the same two-tier lookup
(`dispatch_routine_method`/`dispatch_sub_method` in
`src/runtime/methods_sub.rs`, factored into a shared
`multi_candidate_arity_count` helper in
`src/runtime/methods_signature_candidates.rs`).

## Verification

- New regression test `t/begin-selective-import-proto-multi.t` (8
  assertions, green under real `raku`): a local module
  (`t/lib/ProtoMultiCapture.rakumod`, a plain `proto`+`multi` routine, unaffected
  by which Test provider is active) across all four combinations of
  BEGIN/no-BEGIN and single-variable/list-destructured binding, plus the
  original `Test.rakumod` scenario itself toggled through `MUTSU_REAL_TEST`.
  All four local-module shapes reproduced the bug before the fix and pass
  after it.
- `roast/S32-list/skip.t` passes under `MUTSU_REAL_TEST=1` (previously the
  file never ran a single assertion, dying at its first `plan` call) and
  continues to pass under the native provider.
- `make test` green; a targeted native-provider roast sweep over
  `S32-list`, `S11-modules`, `S10-packages`, `S02-names`, `S06-*`,
  `S04-phasers`, `integration` green; `scripts/battery-testsuite.sh` gate
  passed.

See `todo/deep/vendor-real-test-module.md` for the campaign this closes a row
in.
