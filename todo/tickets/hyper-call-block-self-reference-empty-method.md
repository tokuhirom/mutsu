# `».&?BLOCK` (hyper-call with a block self-reference) dispatches an empty method name instead of calling the block

Discovered via the doc-diff harness on `raku-doc/doc/Language/variables.rakudoc` (around line
1551).

## Repro

```
for 'tmp/ddhtest' {
    .Str.say when !.IO.d;
    .IO.dir()».&?BLOCK when .IO.d
}
```

(with `tmp/ddhtest/` containing a subdirectory holding one file, so the block recurses once)

- raku: prints the file's path (`tmp/ddhtest/sub1/file.txt`) — `&?BLOCK` refers to the
  currently-executing block, and `».&?BLOCK` hyper-calls it recursively over each directory
  entry
- mutsu: `No such method '' for invocant of type 'IO::Path'` — the hyper-call `».&NAME` syntax
  with `&?BLOCK` as the "method" seems to be parsed/dispatched as a method call with an **empty**
  method name, rather than recognizing `&?BLOCK` as a callable to invoke on each hyper'd element.

## Root cause guess

`».&EXPR` (hyper meta-op combined with the `&`-sigil "call this Callable as a method" syntax) is
presumably implemented for a named sub (`».&some-sub`) but the parser/compiler doesn't recognize
`&?BLOCK` (the "current block" pseudo-variable) in that position, and instead falls through to
a plain method-call path with an empty name extracted.

## Affected files (starting point)

- `src/parser/` / `src/compiler/expr.rs` — `».&EXPR` hyper meta-op parsing, specifically the
  `&?BLOCK` pseudo-variable case
- `src/vm/vm_call_ops.rs` — hyper method-call dispatch

## Suggested next step

Check whether a *named* sub already works in this position (`».&some-named-sub`) to confirm the
bug is specific to `&?BLOCK` (or more broadly, any pseudo-variable / non-identifier expression)
rather than the whole `».&EXPR` mechanism being broken.
