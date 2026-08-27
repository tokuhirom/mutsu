# DELIBERATE, NOT A BUG: a shaped multidim array's out-of-range coordinate answers `Nil` under mutsu, where plain (non-PREVIEW) `raku` answers `()`

**This is a deliberate divergence adopted to satisfy the vendored roast suite, not a bug to "fix
back."** Found while fixing `todo/tickets/multidim-value-adverb-hole-returns-nil-not-empty-list.md`
(see `news/2026-08/multidim-value-adverb-hole-shape.md`). If you are reading this because you found
this behavior and want to "correct" it to match plain `raku`, read the whole file first -- doing so
will regress two whitelisted roast tests (cited below).

## (a) Exactly what each side answers

```
# plain raku, default/non-PREVIEW language version, no `use` pragma at all:
$ raku -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:v).raku;'
()

$ raku -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:k).raku;'
()

# mutsu, identical code, no pragma:
$ target/debug/mutsu -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:v).raku;'
Nil

$ target/debug/mutsu -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:k).raku;'
Nil
```

So for a SHAPED array's out-of-range coordinate specifically, `:v`/`:k`/`:p` answer `Nil` under
mutsu where plain `raku` answers `()`. (An in-bounds Array hole -- e.g. `@a[0;1]` on the same
`@a[2;2]` before it is ever assigned -- is NOT affected: both mutsu and `raku` answer `()` there,
matching `t/typed-array-hole-adverbs.t`'s "multidim: :v on a hole is empty" assertions.)

## (b) The roast files/assertions that force this

- `roast/S32-array/multislice-6e.t` line 153-158, under `use v6.e.PREVIEW`:
  ```raku
  non-assignable-ok @array[$a;$b;$c]:k:$delete,
    $exists ?? $abc !! Nil,
    "\@array\[$araku;$braku;$craku]:k:{
        ":delete" if $delete
    } gives {$exists ?? $abcraku !! "Nil"}";
  ```
  exercised (among other rows) with `@array[0;0;3]` and `@array[0;1;0]` -- both out-of-range
  coordinates into `@array = [[[42,666,[314]],],]`, a plain nested/autoviv array, not a shaped one.
  The equivalent `:p`/`:v` assertions are immediately adjacent in the same file.
- `roast/S32-hash/multislice-6e.t` line 113 (same `non-assignable-ok ... :k:$delete, $exists ?? ...
  !! Nil` shape) pins the identical `Nil` answer for a genuinely missing Hash key.

Both files are on `roast-whitelist.txt` and must keep passing (`make roast` is CI-gating). An
intermediate version of this fix that tried to give the SHAPED-array-out-of-range case `()` instead
of `Nil` (to match plain `raku`) broke `roast/S32-array/multislice-6e.t`'s assertions above -- see
`news/2026-08/multidim-value-adverb-hole-shape.md` for that dead end.

## Why one rule can't satisfy both

mutsu does not currently branch multidim-adverb behavior on the language-version pragma (`use v6` vs
`use v6.e.PREVIEW`), so both of these must share ONE answer for `:v`/`:k`/`:p` on a miss:

1. A missing Hash key, or an out-of-range/non-numeric Array coordinate -- both are a bare
   `Value::NIL` with no hole marker of their own (`ArrayData::hole_at` never fires). Roast (under
   `v6.e.PREVIEW`) pins this to `Nil`.
2. An in-bounds Array hole -- carries its own non-`Nil` hole marker (e.g. `Package("Any")`). Both
   mutsu and plain `raku` agree this is `()`.

Since case 1 already covers "missing Hash key" (needs `Nil`, no dispute) and "out-of-range Array
coordinate" with the SAME raw representation, giving them different answers would need mutsu to
distinguish the two by something other than the value itself (e.g. re-deriving which container
kind failed to resolve) -- an earlier attempt at exactly that regressed roast (see above). Given
that, the fix picked ONE answer (`Nil`) for all of case 1, favoring roast (authoritative, CI-gating
per CLAUDE.md) over plain `raku`'s narrower, non-PREVIEW-only preference for `()` on a shaped
array's specific out-of-range sub-case.

## Why this might be worth revisiting (not urgent)

A full fix would need mutsu to track (or re-derive) which language-version pragma was in effect for
the specific piece of code doing the multidim read, and branch the miss-to-`Nil`-vs-`()` decision on
it -- non-trivial, since the version pragma is a parse-time/compile-time fact that would need to
reach this runtime builtin. Given the narrowness of the affected case (an out-of-range multidim
coordinate on a SHAPED array specifically, read under the plain/default language version -- most
real code either doesn't hit this at all or already runs under some `use v6.x` pragma), this was
judged not worth a larger change as part of the original ticket.

**Revisit only if:** a roast test surfaces that actually depends on the `()` answer under the
default (non-PREVIEW) version for this exact case, or per-version branching for multidim semantics
becomes needed for an unrelated reason (at which point this narrow case would be folded in for
free). Until then, leave the current `Nil` behavior alone -- it is correct for what CI checks.

## Pinned in-repo

`t/typed-array-hole-adverbs.t`'s "out-of-range coordinate" block asserts the CURRENT (`Nil`)
behavior deliberately, with a comment pointing at this file.
