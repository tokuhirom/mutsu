# `Range.minmax` on a Range with excluded ends should throw `X::AdHoc`, not silently return bounds

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Range.rakudoc:284`).

## Root cause

Per raku, calling `.minmax` on a `Range` whose upper (or lower) bound is excluded
(`..^`, `^..`, `^..^`) is an error — `minmax` can't represent an excluded bound as a
concrete value, so it throws `X::AdHoc: Cannot return minmax on Range with excluded ends`.
mutsu instead just returns the plain `(min max)` pair, ignoring exclusivity.

```raku
my $r4 = (1.1..^5.2);
say $r4.minmax;
CATCH { default { put .^name, ': ', .Str } };
```

- `raku`: `X::AdHoc: Cannot return minmax on Range with excluded ends`
- `mutsu` (`target/debug/mutsu`): `(1.1 5.2)` (no error)

## Minimal repro

```raku
say (1.1..^5.2).minmax;
```

## Affected files (starting point)

`Range.minmax` implementation — likely in `builtins/methods_0arg/` (Range methods).
Needs to check the Range's `excludes-min`/`excludes-max` flags and throw
`X::AdHoc` (via the standard exception-raising helper) when either is set, matching the
existing message text.
