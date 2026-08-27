# A shaped multidim array's out-of-range coordinate answers `Nil` under mutsu, but plain (non-PREVIEW) `raku` answers `()`

Found while fixing `todo/tickets/multidim-value-adverb-hole-returns-nil-not-empty-list.md` (see
`news/2026-08/multidim-value-adverb-hole-shape.md`). Not a regression from that fix -- it is a
deliberate tradeoff made while landing it, recorded here so it does not get lost.

## The tension

Two multidim (`;`-separated) "missing leaf" cases produce the *identical* raw representation
(`Value::NIL`, `is_hole = false`) with no way to tell them apart from the value alone:

1. A missing Hash key (`%h{"a";"x"}` when `"x"` was never set).
2. An out-of-range (or non-numeric) Array coordinate (`@a[5;5]` on a `my @a[2;2]`, or a coordinate
   past an autoviv array's own current length).

mutsu does not currently branch multidim-adverb behavior on the language-version pragma
(`use v6` vs `use v6.e.PREVIEW`), so both cases must share ONE answer for `:v`/`:k`/`:p` on a miss.
The two things that need that answer disagree:

- The vendored roast test `roast/S32-array/multislice-6e.t` (`use v6.e.PREVIEW`) pins case 2 (an
  out-of-range index into a plain nested/autoviv array) to `Nil` -- e.g. `@array[0;0;3]:k` where the
  innermost array only has 3 elements. `roast/S32-hash/multislice-6e.t` pins case 1 to `Nil` too.
- Plain, non-PREVIEW `raku` (`raku -e 'my @a[2;2]; @a[0;0]=1; say @a[5;5]:v'`, no `use` pragma at
  all) answers `()`, not `Nil`, for a SHAPED array's out-of-range coordinate.

Since roast is authoritative and gates CI (see CLAUDE.md), the fix chose case-1/2 uniformity with
roast's `Nil` answer over matching plain `raku`'s `()` for the narrower case-2/shaped-array
combination. This is pinned (deliberately, with a comment explaining why) in
`t/typed-array-hole-adverbs.t`'s "out-of-range coordinate" block: mutsu answers `Nil`, which
disagrees with plain `raku`.

## Why this might be worth revisiting

An in-bounds Array hole (`ArrayData::hole_at`, e.g. `my @a[2;2]; @a[0;1]` before it is ever
assigned) is unambiguous either way -- it carries its own non-`Nil` hole marker (e.g.
`Package("Any")`), so it is never confused with a Hash miss, and correctly answers `()` under both
mutsu and plain `raku`. Only the narrower "coordinate is out of range entirely" case is affected.

A full fix would need mutsu to track (or re-derive) which language-version pragma was in effect for
the specific piece of code doing the multidim read, and branch the miss-to-`Nil`-vs-`()` decision on
it -- likely non-trivial, since the version pragma is a parse-time/compile-time fact that would need
to reach this runtime builtin. Given the narrowness of the affected case (an out-of-range multidim
coordinate on a SHAPED array specifically, read under the plain/default language version), this was
judged not worth a larger change as part of the original ticket. Revisit if a roast test surfaces
that actually depends on the `()` answer under the default version, or if per-version branching for
multidim semantics becomes needed for an unrelated reason (at which point this narrow case would be
folded in for free).

## Repro

```
# plain raku (default version, no pragma): answers ()
raku -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:v).raku;'   # ()

# mutsu (same code, no pragma): answers Nil, matching roast's v6.e.PREVIEW-pinned answer instead
target/debug/mutsu -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:v).raku;'   # Nil
```
