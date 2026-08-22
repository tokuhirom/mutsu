# `$!.backtrace[N]` positional indexing returns `Nil`, and mutsu's backtrace has fewer frames

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Backtrace.rakudoc:15`).

## Repro

```raku
sub zipi { { { die "Something bad happened" }() }() };
try {
    zipi;
}
if ($!) {
    say $!.backtrace.elems;
    say $!.backtrace[0];
    say $!.backtrace[*-1];
}
```

- `raku`: prints `7`, then two `Backtrace::Frame.new(...)` gists (one per index).
- `mutsu` (`target/debug/mutsu`): prints `4`, then `Nil` twice — positional indexing into
  the `Backtrace` object always returns `Nil`, regardless of index.

## Root cause hypothesis

Two separate but related gaps:

1. `Backtrace` positional indexing (`$bt[N]`, including the `*-1` whatever-index form)
   is not implemented/dispatched — it falls through to a default that returns `Nil`
   instead of returning the `Backtrace::Frame` at that position.
2. mutsu's captured backtrace has fewer frames than raku's (4 vs 7) — raku includes
   internal setting frames (e.g. `Exception.throw`) that mutsu's frame capture skips.
   This is a secondary, lower-priority difference (mutsu's frame model is inherently
   different from Rakudo's), but is worth noting since it changes what index `N` even
   refers to.

The `.raku` call on the indexed frame in the doc's original example
(`$!.backtrace[*-1].raku`) never even gets reached because the indexing itself already
returns `Nil`.

## Affected files (starting point)

- Wherever `Backtrace` is implemented as a builtin type (grep for `"Backtrace"` in
  `src/runtime/` and `src/builtins/`) — the positional/subscript dispatch for this type.
