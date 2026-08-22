# `Str.match(pattern, :x(BAD_TYPE))` doesn't validate the `:x` adverb's value type

Discovered via the doc-diff harness on `raku-doc/doc/Type/X/Str/Match/x.rakudoc` (around line
15) — the exception type `X::Str::Match::x` this doc page documents is never thrown at all.

## Minimal repro

```raku
say "foobar".match("o", :x<hello>);
CATCH { default { put .^name, ': ', .Str } };
```

- `raku`: `X::Str::Match::x: in Str.match, got invalid value of type Str for :x, must be Int or
  Range`
- `mutsu` (`target/debug/mutsu`): `｢o｣` — the `:x` adverb is silently ignored (the match runs as
  if `:x` weren't passed at all) instead of validating its value type and throwing.

## Root cause

`.match`'s `:x` adverb (ordinal-shortcut match selection, already implemented per the Resolved
entry "`Str.rakudoc` [match] — `.match(/../, :1st/:2nd/:Nth)` ignored the ordinal adverb
shortcuts") accepts an `Int` or `Range` value. mutsu's implementation apparently doesn't
type-check the `:x` value before using/ignoring it — it needs to check that the passed value is
`Int` or `Range` and throw the registered `X::Str::Match::x` exception (already present in
`runtime_init.rs`'s exception-type registry, per the doc's own class name existing in mutsu's `X`
hierarchy — worth confirming) when it isn't.

## Affected files (starting point)

- `src/runtime/` — wherever `.match`'s `:x`/`:1st`/`:2nd`/`:Nth` adverb handling lives (see the
  Resolved-section note above for the PR that added the ordinal-shortcut parsing — the same
  function needs a type-check on the resolved `:x` value before use)
