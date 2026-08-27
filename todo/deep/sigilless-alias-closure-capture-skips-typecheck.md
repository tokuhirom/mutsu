# Writing through a sigilless bind alias captured into a closure still skips the type check

## Measured 2026-08-27 (`main` @ `10ac4d450`): the title is wrong — this is a write-through bug, and it is not sigilless-specific

The "Root cause (not yet investigated)" section below guessed correctly that
this might be "a write-through bug first, type-check bug second". It is, and
the type constraint is not involved at all. It is also **not specific to a
sigilless alias** — an ordinary `$`-sigil `:=` alias fails identically, which
the original repro did not reveal because it used a typed variable:

| # | program | mutsu | raku |
| --- | --- | --- | --- |
| D2 | `my $s = "a"; my $t := $s; my $f = { $t = 42 }; $f(); say $s` | `a` | `42` |
| D4 | `my $s = "a"; my \x := $s; { x = 42 }(); say $s` | `42` | `42` |
| D5 | `my $s = "a"; my \x := $s; sub f { x = 42 }; f(); say $s` | `a` | `42` |

D2 is untyped and uses `$t`, not `\x`, and still loses the write. D4 (the
immediately-invoked block) agrees, so the discriminator is **stored/deferred
closure vs. immediate invocation**, not the sigil and not the type. The
type-check divergence in the original repro is downstream: the write never
reaches `$s`, so there is nothing left to type-check.

**Retitle when fixing.** The accurate statement is: *a `:=`-bound alias stops
aliasing when the write happens inside a closure that is stored and called
later.*

### Likely one family with two neighbouring findings

All three are about how a `:=` bind reaches a frame other than the one it was
declared in, which today is `Interpreter::propagate_bind_to_ancestor_frames`
(`src/vm/vm_var_assign_ops.rs`) — a **name-based** ancestor-frame splice:

- [bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals](bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md)
  — the bind reaches frames it must **not** (unrelated recursive invocations).
- `todo/tickets/routine-local-bind-writes-through-to-same-named-outer-lexical.md`
  — the bind leaks **out** to a same-named caller lexical.
- This finding — the bind fails to reach a frame it **must**.

"Reaches a frame it must not" and "fails to reach a frame it must" are
plausibly the same missing identity token. Check the other two before scoping
this one separately.

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
