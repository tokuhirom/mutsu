# A block's closure capture now sees free variables read only by a nested named `sub`

A block or `sub {...}` literal's compile-time free-variable scan (`CompiledCode::compute_free_vars`
in `src/opcode.rs`) already folded a nested ANONYMOUS closure's free vars into its own capture set —
the mechanism that lets `{ my $x = 1; { $x } }` capture `$x` transitively — but never did the
equivalent for a nested NAMED `sub`. A named sub is registered via `RegisterDecl` (compiled by
`Compiler::compile_sub_body_with_deprecation` in `src/compiler/helpers_sub_body.rs`) into a completely
separate `CompiledFunction`, not embedded in `closure_compiled_codes`, so the enclosing scan simply
never saw which outer lexicals its body referenced:

```raku
my $l = 42;
my &blk = { sub nested() { say "l=$l" }; nested(); };
blk();
```

- raku: `l=42`
- mutsu (before this fix): `Use of uninitialized value element of type Any in string context.` then
  `l=`

The failure was silent rather than a hard error, which made it easy to miss — it surfaced while
implementing `Lock::Async.protect-or-queue-on-recursion`
(`news/2026-08/lock-async-recursion-methods-missing.md`): a helper `sub` declared inside the protected
block lost access to the lock variable it needed to re-enter.

## Fix

`CompiledCode` gained a new field, `named_sub_free_reads: Vec<Vec<Symbol>>`, populated in
`compile_sub_body_with_deprecation` right alongside the existing `named_sub_captures` (which already
tracked a nested named sub's WRITES, to drive shared-cell boxing). Each entry is the finalized nested
sub's own `free_var_syms` — its full read+write free-variable set, already computed relative to its
own locals and parameters. `compute_free_vars` folds these into the enclosing scope's `free` set the
same way it already folds a nested closure's `free_var_syms`, right before the closure-capture set is
finalized. Because the fold happens generically at every `SubDecl` compile site, it is automatically
transitive through multiple levels of nesting and independent per `multi sub` candidate (each
signature is compiled — and folded — separately).

Deliberately narrow: the fold only widens `free` (the ordinary capture set). It does not touch
`self_mutated`/`free_writes`, since named-sub mutation tracking (which drives shared-cell boxing for
cross-call accumulation) is already handled by the pre-existing `named_sub_captures` channel and did
not need to change.

## Verified matrix (raku v2026.06 vs mutsu)

All of the following pass identically under both interpreters (pinned in
`t/closure-capture-nested-named-sub.t`):

- a nested named sub reading an outer `my` (the repro), an outer `our` (package storage, unaffected
  by this fix), and a sigilless alias (`my \x = ...`)
- **negative controls**: the nested sub's own `my`-redeclaration of the same name, and its own
  parameter of the same name, both correctly shadow the outer lexical instead of writing through it
- two levels of named-sub nesting (`sub a { sub b { $l } }`) — the fold is transitive
- `multi sub`: each candidate signature independently contributes its free vars
- a named sub nested inside a `sub {...}` literal, not just a `{...}` block
- mutation (`$n++` from inside the nested sub) — pre-existing, unaffected by this fix, pinned so it
  can't regress alongside the read fix
- an own-block local read by a nested sub (a different, pre-existing mechanism —
  `compute_needs_env_sync`'s conservative lazy-body env-sync gate) — pinned for the same reason
- in-place container mutation (`@a.push(...)`) of an outer array from inside a nested sub
- the `Thread`/`clone_for_thread` path (an escaping closure handed to `Thread.start` already
  snapshots more conservatively, so this case turned out to be unaffected either way — but is pinned
  as coverage since the ticket named it as a related channel)
- `Lock::Async.protect-or-queue-on-recursion`, the real-world shape that surfaced the bug: a nested
  named sub re-enters the same lock via a captured outer variable now genuinely works, matching raku.
  (The ticket's own repro snippet used plain `.protect` instead, which — verified against real raku —
  actually deadlocks recursively in both raku and mutsu; that was a mistake in the original ticket,
  not a difference between the two. `protect-or-queue-on-recursion` is the correct reentrant-safe
  method and is what the doc example actually demonstrates.)

## A methodological trap this ticket's own test-writing ran into

A first draft of the regression test wrapped each case in its own bare `{ ... }` test-scope block
(the usual style in this file's siblings). That accidentally made 10 of 15 assertions pass even
*without* the fix: a plain top-level `{ ... }` statement compiles through `OpCode::BlockScope`, whose
conservative env-sync gate (`env_consumer_slots.block_scope` in `compute_needs_env_sync`)
unconditionally syncs every local the block's body touches into the name-keyed env — for every such
bare block *except the textually-last one* in the compilation unit (which instead compiles through
the lighter tail-position `compile_bare_block_inline`, with no such gate). That happened to make a
nested named sub's outer-var read resolve correctly regardless of this fix, for every case except the
last. The final test file keeps every case flat (no bare-block wrapper), relying only on per-case
unique variable/sub names for isolation, which was confirmed (by temporarily reverting the fix) to
make each assertion fail independently as expected.

## Related, unfixed gap filed separately

A method inside a `class` declared inside a block hits the same symptom through a completely
different compile path (`Compiler::compile_method_body`, not `compile_sub_body_with_deprecation`),
which has no equivalent fold. Filed as
`todo/tickets/method-in-class-declared-inside-block-misses-outer-var.md` rather than folded into this
fix, since it needs its own compile-path-specific change.
