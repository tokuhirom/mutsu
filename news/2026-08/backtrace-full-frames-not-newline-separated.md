# `Backtrace.full` renders one frame per line again

`$!.backtrace.full` concatenated every frame onto a single line, so a backtrace
that should read as

```
  in sub f at f.raku line 1
  in block <unit> at f.raku line 2
```

came out as `  in sub f at f.raku line 1  in block <unit> at f.raku line 2`.
Measured against reference `raku`, `sub f { die "x" }; try { f() }; say
$!.backtrace.full.lines.elems` answered **5** there and **1** in mutsu. The
finding came from the doc-diff harness (`Type/Exception.rakudoc:78`).

## Root cause

Not a missing join in `.full`, as the ticket had guessed — the missing newline
was one level down, in the frame renderer itself.

Rakudo's `Backtrace::Frame.Str` is **newline-terminated**: a frame stringifies
to `"  in sub f at f.raku line 1\n"`, not to the bare text. That is precisely
what makes `Backtrace.full`, `.concise` and `.summary` — all three of which are
documented as plain *concatenations* (`.grep(...).join`, no separator) of the
frame strings — come out one frame per line. mutsu's `backtrace_frame_str`
returned the text without the terminator, so the three concatenations ran the
frames together and `Backtrace::Frame.Str` itself was a line short of Rakudo's.

Adding the separator inside `.full` alone would have "fixed" the symptom while
leaving `.concise`/`.summary`/`Frame.Str` wrong *and* breaking the identity the
spec (and `roast/S32-exceptions/misc.t`) asserts, namely that `.concise` equals
`$bt.grep({ !.is-hidden && .is-routine && !.is-setting }).join`. Terminating the
frame string fixes all four at once and keeps that identity exact.

A second, smaller divergence surfaced in the same renderer: a frame with no
subname is an anonymous block, and Rakudo renders it `  in block  at ...`. mutsu
rendered it `  in sub  at ...`. The renderer now picks `block` for both the
empty subname and the synthetic `<unit>` frame, `sub` otherwise.

`Backtrace.Str` and the `text` attribute are deliberately untouched. That
attribute is what `Interpreter::exception_backtrace_text` reads to print an
uncaught error, so a trailing newline there would ripple through every error
message in the interpreter; Rakudo's `.Str` does end with a newline, and that
remains a known, separately-scoped difference.

## Coverage

`t/backtrace-introspection.t` pins the line-per-frame shape structurally —
`.full.lines.elems == .elems`, `.full` newline-terminated, every line matching
`  in ... at FILE line N`, and `.full eq .list.map(*.Str).join` — so it holds
under both `raku` and `mutsu` despite the two having different frame counts (see
`todo/tickets/backtrace-frame-indexing-returns-nil.md`).
