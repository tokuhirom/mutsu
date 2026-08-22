# `.snitch` (v6.e.PREVIEW debugging method) is unimplemented

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc` (around lines 1549,
1559).

## Repro

```
use v6.e.PREVIEW;
(1..5).snitch;
```

- raku: prints `1..5` (to `$*ERR` by default) and returns `self` unchanged
- mutsu: `Unknown function: snitch`

The writable-attribute form is also missing:

```
(my $a = 42).snitch = 666;
```

- raku: sets the "what to print" formatter/behavior (per the doc, `.snitch` exposes a settable
  attribute controlling its output)
- mutsu: `X::Assignment::RO: cannot assign through .snitch on non-instance`

## Root cause

Simply unimplemented — `.snitch` is a `v6.e.PREVIEW`-gated debugging method on `Any` (like
`.say`/`.note` but mid-expression-transparent: it returns its invocant unchanged after printing
it, so it can be spliced into a chain for debugging). Needs a native 0-arg method (and possibly
a writable-attribute variant per the doc) added under whatever `v6.e.PREVIEW`-only method gating
mutsu already uses for other 6.e.PREVIEW features (e.g. `.snip`, global `rotor()`).

## Affected files (starting point)

- `src/builtins/methods_0arg/` — where other `Any`/`Cool` 0-arg methods live
- Grep for how other `v6.e.PREVIEW`-only builtins (e.g. `.snip`) are gated in the codebase

## Suggested test

`t/snitch.t`, verifying invocant pass-through and stderr output (per the doc's description) for
at least one representative type (Int/Range/Str).
