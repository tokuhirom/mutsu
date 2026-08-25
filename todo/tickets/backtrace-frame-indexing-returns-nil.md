# mutsu's backtrace has fewer frames than Rakudo's

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Backtrace.rakudoc:15`).

## Status

This ticket originally covered two gaps. **Gap 1 (positional indexing) is
fixed** — see `news/2026-08/backtrace-positional-indexing.md`. Only gap 2, the
frame-count difference, remains open, and it is deliberately deferred (see
"Why this is deferred" below).

- [x] **Gap 1 — `$bt[N]` returned `Nil`.** Positional indexing into a
  `Backtrace` always answered `Nil`/`Any` regardless of the index, because no
  arm of the subscript dispatch knew about the `frames` attribute. The
  subscript now delegates to the stored `frames` List, so every index shape
  (single index, `[*-1]`, `[0,1]` slices, `[^2]`/`[0 .. *-1]` ranges, `[*]`,
  and out-of-range reading back as `Nil`) behaves exactly as it does on a
  List. `$bt.AT-POS($i)` was implemented alongside it. Pinned by
  `t/backtrace-positional-index.t`, which passes unmodified under both `raku`
  and `mutsu`.
- [ ] **Gap 2 — fewer frames than Rakudo.** Still open, lower priority.

## Repro (gap 2)

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

## Related smaller divergence noticed while fixing gap 1

`Backtrace::Frame.gist` / `.raku` do not match Rakudo's rendering:

- `raku`: `Backtrace::Frame.new(file => "...", line => 3, code => -> { ... }, subname => "<unit>")`
- `mutsu` `.gist`: the frame's `.Str` text (`  in block <unit> at f.raku line 3`)
- `mutsu` `.raku`: a bare `Backtrace::Frame.new` with no attributes

`.Str` itself matches Rakudo — including its trailing newline, since
`news/2026-08/backtrace-full-frames-not-newline-separated.md`. Faithfully
reproducing the gist is partly impossible (Rakudo's `code =>` renders a `Block`
with its memory address) and partly the same frame-model question, so it is
recorded here rather than fixed.

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
