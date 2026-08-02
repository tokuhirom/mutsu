# Two inline `start` blocks write their stale captured env over a variable declared after them

Extracted from PLAN.md §6 (2026-08-02); found 2026-07-23 while testing WASM concurrency, and
re-verified on `main`2026-08-02 — it is not a WASM artefact.

## Repro

```raku
my $p = Promise.allof(start { 1 }, start { 2 }); await $p; say $p.WHAT;
# mutsu: Nil        raku: (Promise)

my @p = (start { 1 }, start { 2 }); my $q = Promise.anyof(@p); await $q;
# $q survives
```

## Root cause

Two or more `start` blocks written **inline as arguments** each capture the enclosing env at a point
where `$p` does not exist yet, so their snapshot records it as `Nil`. Joining them writes that
snapshot back wholesale, clobbering the now-assigned `$p`.

One inline `start` does not trip it, and binding the promises to a variable first avoids it — i.e.
this is the blanket env writeback, the same mechanism as the `named-sub free-var shadow` /
dual-store family, **not** a `Promise` bug.

## The fix belongs elsewhere

With the cell-based capture work (write back only what the thread actually mutated), not as a
special case at the `allof` / `anyof` call sites. See
[needs-env-sync-blanket-removal.md](../deep/needs-env-sync-blanket-removal.md) and
[closure-env-capture-cost.md](../deep/closure-env-capture-cost.md).
