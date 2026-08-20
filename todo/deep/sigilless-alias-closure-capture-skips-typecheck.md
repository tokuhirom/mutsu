# Writing through a sigilless bind alias captured into a closure still skips the type check

## Symptom

`news/2026-08/sigilless-alias-write-now-type-checked.md` fixed the type check
for a write reaching a typed scalar through a sigilless `:=` bind alias, for
the direct (inline-block) case and for a sigilless routine parameter that
aliases a typed caller variable. It does NOT cover an alias that has been
captured into a genuine closure:

```raku
my Int $a = 5;
my \x := $a;
my &blk = sub { x = "not an int" };
blk();
say "a=$a";
```

Raku: dies with `Type check failed in assignment to $a; expected Int but got
Str ("not an int")`.

mutsu (after the fix above): prints `a=5` with no error — the write silently
does nothing at all (not even the untyped write-through works; see below),
so this is arguably two bugs layered together.

An inline block (not passed anywhere as a callable) is NOT affected — this
works correctly today:

```raku
my Int $a = 5;
my \x := $a;
{
    x = "not an int";   # correctly dies
}
```

And a sigilless routine PARAMETER (`sub f(\x) { x = ... }; f($a)`) is also
NOT affected — that already works, because the write happens to `x`'s own
local slot inside `f`'s frame and goes through the same direct `SetLocal`
alias-chain-walk code path the fix patches, not a closure capture.

## Root cause (not yet investigated)

A closure created via `sub { ... }` (or `-> { ... }`, `{ ... }` passed as a
callable, e.g. `throws-like { ... }`) captures outer lexicals through a
different mechanism than a plain nested block — some form of cell/box
capture (see `docs/captured-outer-cell-sharing.md` and the closure-escape
machinery in `src/vm/vm_closure_dispatch.rs`). The fix in
`src/vm/vm_helpers.rs` (`check_sigilless_alias_target_constraint`) is wired
into the direct `SetLocal` write-through call sites
(`src/vm/vm_var_assign_set_local.rs`, `src/vm/vm_var_assign_local.rs`); it is
very likely NOT reached when the write happens through a captured cell
instead of a local slot.

Worth checking first: does the untyped case even work (`x = "now a string"`
propagating to `$a` when captured into a closure)? The repro above shows NO
error AND no propagation (`$a` stays `5`), which suggests the write to a
closure-captured sigilless alias may not reach the source variable at all
today, independent of typing — i.e. this may be a write-through bug first,
type-check bug second, mirroring the shape of
`todo/deep/for-loop-pointy-sigilless-param-write-through-missing.md`.

## Minimal repro

```raku
my Int $a = 5;
my \x := $a;
my &blk = sub { x = "not an int" };
blk();
say "a=$a";   # raku: dies at the assignment inside blk(); mutsu: prints "a=5", no error
```
