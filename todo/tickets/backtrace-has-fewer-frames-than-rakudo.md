# mutsu's backtrace has fewer frames than Rakudo's

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Backtrace.rakudoc:15`).

## Status

This ticket originally covered three gaps:

- **Gap 1 — `$bt[N]` returned `Nil`.** Fixed — see
  `news/2026-08/backtrace-positional-indexing.md`.
- **`Backtrace::Frame.gist`/`.raku` divergence.** Fixed — see
  `news/2026-08/backtrace-frame-gist-raku-attribute-shape.md`.
- **Gap 2 — fewer frames than Rakudo.** Still open (this file), deliberately
  deferred — see "Why this is deferred" below.

## Repro

```raku
sub zipi { { { die "Something bad happened" }() }() };
try {
    zipi;
}
if ($!) {
    say $!.backtrace.elems;
}
```

- `raku`: prints `7`.
- `mutsu`: prints `4`.

Rakudo counts internal setting frames that mutsu never captures. In the repro
above, `raku`'s frame 0 is

```
Backtrace::Frame.new(file => "SETTING::src/core.c/Exception.rakumod", line => 65,
                     code => method throw (...), subname => "throw")
```

whereas mutsu's frame 0 is already the user-level `<unit>` block. The user-level
frames themselves line up; Rakudo simply has more of them below and above.

## Why this is deferred

mutsu's frame model is inherently different from Rakudo's: mutsu has no CORE
setting written in Raku, so there is no `SETTING::src/core.c/*.rakumod` frame to
report, and its native builtins are Rust functions rather than Raku routines
with callframes. Matching Rakudo's count would mean synthesizing frames for
interpreter internals that have no natural mutsu equivalent — a much larger and
more architecturally invasive change than the indexing fix, and one that would
make `.gist`/`.full` output *less* useful for mutsu users rather than more.

The practical consequence is that a given index `N` refers to a different frame
in mutsu than in Rakudo. Code that indexes from the end (`[*-1]`) is unaffected;
code that hardcodes a small index expecting a setting frame will differ.

## What the introspection work relies on this ticket for

`Backtrace.next-interesting-index` / `.outer-caller-idx` / `.nice` landed in
`news/2026-08/backtrace-introspection-methods-missing.md`, deliberately defined
*relative to mutsu's own frame list* rather than to Rakudo's absolute indices:
`nice` starts at frame 0 (Rakudo starts at 1, because its frame 0 is always a
setting frame), and `outer-caller-idx` reconstructs the lexical chain from the
dynamic stack. Closing gap 2 would move those entry points, not invalidate them.

## Affected files (starting point)

- `src/vm/vm_helpers.rs` — `build_backtrace_value` / `backtrace_value_from_string`
  build the frame list; this is where extra frames would have to originate.
