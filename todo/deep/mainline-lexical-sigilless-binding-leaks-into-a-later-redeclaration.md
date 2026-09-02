# A named sub closing over a sigilless binding leaks that binding into a later same-named declaration

## Repro (measured 2026-09-02, `main` + the `bind-alias-is-a-container` change)

```raku
use Test;
plan 3;
{
    my $s = "a";
    my \x := $s;
    sub named-writer { x = 42 }     # `x` becomes a MAINLINE lexical (ADR-0024)
    named-writer();
    is $s, 42, 'named sub writes alias';
}
{ my \x := 5; dies-ok { x = 9 }, 'literal immutable'; }
{ my \x := 5; my $e; { x = 9; CATCH { default { $e = .message } } };
  like $e, /'immutable Int'/, 'names value'; }
```

raku: 3/3 pass. mutsu: test 1 passes, **tests 2 and 3 fail** — the second
block's `my \x := 5` binds a plain `Int`, so `x = 9` must die with "Cannot
modify an immutable Int (5)", but the write silently succeeds and `$e` stays
undefined.

Drop the first block and both remaining tests pass, so the first block's
declaration is what poisons the later one.

## Why

ADR-0024 gives a mainline named sub a `unit_lexicals[MAINLINE_UNIT_KEY]` entry
for each lexical it closes over, so `named-writer` resolves `x` through a shared
cell rather than through whatever env the caller happens to have. That entry is
keyed by the bare NAME and survives the block that declared it. The later,
unrelated `my \x := 5` in a sibling block therefore still finds a
`ContainerRef` under `x`:

- `MarkSigillessBind` (which settles a sigilless term's mutability from what it
  is bound to) sees a container and leaves the term writable;
- the write then goes through that stale cell instead of being refused.

The two blocks are different lexical scopes, so raku's `x`s are unrelated
variables. mutsu's mainline capture store has no scope discriminator for them.

## Scope

This is ADR-0024 territory (`docs/adr/0024-mainline-lexicals-for-named-subs.md`),
not a sigilless-binding bug: the sigilless term is merely the reader that makes
it visible. The same store is consulted by `mainline_lexical_cell` from the
`:=` bind path and by `inject_mainline_lexical_captures` at closure creation, so
a fix has to decide what identity a mainline lexical entry carries beyond its
name — most likely the declaring slot, the way
`needs_cell_ref_capture_slots` is deliberately slot-addressed rather than
name-addressed (ADR-0032 D2's "slot-addressed, never name-addressed"
constraint, which exists for exactly this failure).

## How it was found

Writing `t/bind-alias-is-a-container.t` for
`news/2026-09/bind-alias-is-a-container-not-a-name.md`: the value-bind
assertions failed only when an earlier section of the same file had put a
same-named sigilless binding into a named sub. The test now uses distinct names
per section, so it does not cover this; the repro above is the coverage.
