# `Map.new(a, 1, :b(2))` — bare colon-pair should bind as a named arg to `.new`, not a positional Pair

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Map.rakudoc:62`).
This is the exact "WRONG" footgun example the doc itself uses to teach the distinction.

## Root cause

A bare `:b(2)` colon-pair passed directly in a call's argument list (no extra
parenthesization) is a **named argument**, not a positional `Pair` — this is general
Raku call-argument parsing and already works correctly for a plain user sub:

```raku
sub f(*@pos, *%named) { say @pos; say %named };
f("a", 1, :b(2));     # raku AND mutsu: @pos=[a 1]  %named={b => 2}   -- matches
```

But `Map.new(...)`'s constructor dispatch treats the same `:b(2)` as a positional `Pair`
argument instead of consuming it as a named arg:

```raku
say Map.new("a", 1, :b(2)).keys;  # raku: (a)     mutsu: (a b)
```

Since the general named-arg mechanism works for ordinary subs, this looks specific to how
`Map.new`'s native/builtin constructor collects its arguments (probably it slurps
everything — positional and named alike — into the pair list instead of only the
positional args).

## Minimal repro

```raku
say Map.new("a", 1, :b(2)).keys;
```

- `raku`: `(a)`
- `mutsu` (`target/debug/mutsu`): `(a b)`

## Affected files (starting point)

`Map.new` construction — likely `dispatch_new_from_pairs` or wherever `Map`/`Hash`
`.new(...)` collects its constructor arguments (search for `Map.new` handling in
`runtime/` or `builtins/`). Needs to only consume positional args into the pair list and
let named args pass through/be dropped the way a real slurpy `(*@pos, *%_)` signature
would.
