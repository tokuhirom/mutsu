# Two unrelated frames sharing a container name collide through the cross-thread store

**Status 2026-08-20: still live, but re-scoped. Design is owned by
[ADR-0039 §8](../../docs/adr/0039-container-lexicals-resolve-lexically.md).**
This file is now the *evidence* record; do not start work from the original
proposal below, which is superseded.

## Where this stands

Re-measured on `52631889f`. Two of the three claims this ticket was built on
have expired, and the third has narrowed to a single sigil.

- **"The store is a process-global map" — fixed.** ADR-0010 shipped the
  lineage-chained `SharedStore` (`src/runtime/shared_store.rs:55-61`). Sibling
  spawns no longer see each other's lexicals; verified against `raku` with three
  concurrent workers each declaring `my @w`.
- **"Re-key the store" (this ticket's proposed fix) — rejected.** It is both
  already done and insufficient: the live repro below collides two frames of
  *one* thread inside *one* lineage. No keying discipline short of per-frame
  keys — that is, slots — removes it. ADR-0039 §8.4 records the rejection.
- **Scalars — clean.** Ten shapes were probed (a callee's `while --$i`
  countdown, a callee that spawns and then writes its `my $i`, `is copy`
  parameters, `for` parameters, Nil-valued and live-valued readers). All match
  `raku`. The `thread_redeclared_vars` mask plus the Nil-gated `GetLocal` pull
  cover the scalar lane.
- **Containers (`@`/`%`) — still broken.** This is the whole of what is left.

## The live repro

Deterministic, no modules, no concurrency — one `await start { 1 }` anywhere in
the process is enough to arm the lane, and the collision then repeats on every
call.

```raku
sub work($tag) {
    my @items = ($tag,);
    await start { 1 };          # delete this line and mutsu is correct
    @items.push("$tag-2");
}
my @items = <x y z>;
work('A');  say @items.raku;    # raku: [x y z]        mutsu: [A A-2]
@items.push('MINE');            # raku: [x y z MINE]   mutsu: [A A-2 MINE]
work('B');  say @items.raku;    # raku: [x y z MINE]   mutsu: [B B-2]
```

A second, independent shape — a **non-slurpy `@`/`%` parameter** escaping its
call, because `mask_thread_redeclared_params`
(`src/runtime/runtime_shared_vars.rs:304-311`) masks only scalars and slurpies:

```raku
sub takes(@list is copy) { await start { 1 }; @list.push('R') }
my @list = <x y z>;
takes(<p q>);
say @list.raku;                 # raku: [x y z]        mutsu: [p q R]
```

Both reproduce identically with `%`, and the first also reproduces through a
`use`d module (the module routine's local `@parts` overwrites the consumer's
`@parts` — the mirror image of
`todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md`, where
the consumer overwrites the module). A `Supply`/tap driver does not arm the
lane; `start` and `Promise` do.

## Root cause, restated

Not the store's keying — **by-name container resolution**, which is
ADR-0039's root cause reached through a second population route.

The scalar and container lanes share the same polluted store and differ only in
how a read resolves the name. A scalar reads its slot and consults the store
only when the slot holds `Nil` (`src/vm/vm_var_assign_local_get.rs:256,268`). A
container has a slot that nothing ever reads, so `GetLocal`'s `@`/`%` arm
consults the store unconditionally — no `is_thread_clone()` gate, no staleness
test (`src/vm/vm_var_assign_local_get.rs:155-161`) — and
`sync_shared_vars_to_env` writes every dirty store key into `env` under the bare
name (`src/runtime/runtime_shared_vars.rs:646-648`), where that read finds it.

What defeats the guard is a **third instance of the `@`/`%` sigil skip** that
ADR-0024 and ADR-0025 already defer and ADR-0039 §4.1 lifts:
`block_captured_scalars` (`src/runtime/runtime_thread.rs:20-22`) skips
`@`/`%`/`&`, so a container is never in `captured_scalars`, so
`clone_for_thread`'s retain (`src/runtime/runtime_thread.rs:352-356`) drops the
container's `thread_redeclared_vars` entry at **every** spawn. After that,
`container_name_is_redeclared` (`src/runtime/runtime_shared_vars.rs:238-242`) —
consulted at nine sites precisely to keep a re-declared container frame-local —
is querying a set the spawn just emptied.

## What to do

Nothing standalone. ADR-0039 §8 folds this in and records what it adds to that
ADR's plan: the parameter shape belongs to slice 2 (do **not** widen
`mask_thread_redeclared_params` — that grows the mechanism slice 2 deletes);
slice 2 must add a container counterpart to the scalar
`pending_caller_var_writeback` drain, or `my @a; await start { @a.push(1) }`
(correct today) silently breaks; and four by-name container mechanisms become
deletable rather than carried forward.

Keep this file open until ADR-0039 **slice 2** lands. Slice 1 alone does not
close it — the containers above are routine-local and parameter-bound, not
file-scope.

## Exposure

No whitelisted roast test and no bundled battery is blocked today. The ticket's
original driver, Cro's `t/http-session-inmemory.rakutest`, was resolved by
unrelated fixes and Cro's test suite is not vendored. The three downstream
tickets this file named — `supply-block-lexical-leaks-through-thread-lane`,
`cue-loop-lexical-shared-lane-residue`, and
`for-multi-param-array-hash-shadow-clobbers-outer-container` — are all resolved.
That is a statement about which shapes the corpus happens to contain, not about
severity: the failure mode is a silent wrong container value in any program that
spawns a thread and reuses a container name.
