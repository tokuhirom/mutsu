# `$*ARGFILES.eof` and `$*IN.eof` now terminate instead of looping forever

The idiomatic "read every line of every argument" loop

```raku
while !$*ARGFILES.eof {
    say $*ARGFILES.get;
}
```

never terminated in mutsu. It printed `Nil` forever and had to be killed. The
same was true of `$*IN.eof`, which stayed `False` even after stdin had been
drained.

## Root cause

`IoHandleState::eof` (`src/runtime/handle_open.rs`) implemented only the
seekable case. Its final readable arm was a blanket

```rust
IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(false),
```

so neither handle target could ever report end-of-input. The originating
ticket guessed this was specific to the "no file arguments, fall back to
stdin" path; it was not. `$*ARGFILES` looped just as forever with one, two, or
any number of real file arguments, and `$*IN.eof` was wrong on its own.

There were two distinct problems behind that one arm:

1. **Non-seekable streams have no position to compare.** Rakudo does not peek
   ahead on stdin either: `$*IN.eof` stays `False` until a read actually came
   back empty, and only then flips to `True`. mutsu had no state recording
   that a read had hit end-of-stream.
2. **`$*ARGFILES` over real files is an `IO::CatHandle`,** whose `.eof` is not
   a property of a single stream: it means "the active source is exhausted
   *and* no further source is left". Answering it needs the `@*ARGS` file
   list, which lives in the env and so cannot be read from inside
   `IoHandleState`.

A third, separate reason the file case stayed broken even once the state knew
better: the VM's compiled IO fast path (`src/vm/vm_call_method_compiled_io.rs`)
dispatches `eof` straight to `IoHandleState::eof` while holding the handle
table, bypassing the interpreter entry point entirely.

## Fix

- Added `IoHandleState::stream_hit_eof`, set wherever a read from stdin (or
  from `$*ARGFILES`'s stdin fallback, or from a `-` entry in the file list)
  comes back empty — covering line reads, byte reads, and char reads.
  `IoHandleTarget::Stdin` now reports `.eof` from that flag.
- Added `IoHandleState::eof_argfiles`, which mirrors Rakudo's
  `IO::CatHandle.eof`. It advances **at most one source per call** (Rakudo's
  `self!next-handle`) and never skips past an empty file, peeking the active
  reader with `BufRead::fill_buf` so nothing is consumed.
  `Interpreter::handle_eof_value` fetches `@*ARGS` before borrowing the handle
  table and routes ArgFiles handles to it.
- The compiled fast path now declines `eof` on an ArgFiles handle
  (`IoHandleState::is_argfiles`) and falls through to the interpreter, which
  can reach the env.

## Behaviour, matched against real `raku`

Every case below was compared against the reference implementation and now
agrees exactly:

| input | result |
| --- | --- |
| no file args, empty stdin | one `Nil`, then stop |
| no file args, stdin with lines | the lines, then one trailing `Nil` |
| one non-empty file | the lines, no trailing `Nil` |
| several non-empty files | all lines concatenated, no trailing `Nil` |
| a single empty file | one `Nil` |
| empty file followed by a non-empty one | the lines, no trailing `Nil` |
| non-empty file followed by an empty one | the lines, then one `Nil` |
| `-` in the file list | stdin is read in place, then one trailing `Nil` |
| `.eof` before any read | `False`, even for an empty file or empty stdin |

## Tests

`t/argfiles-eof-stdin.t` pins all twelve cases. It spawns child interpreters
via `run(..., :in, :out)` so stdin can be controlled per case, and drives every
`$*ARGFILES` loop with a bounded counter so a regression fails the assertion
rather than hanging the suite. The file passes unmodified under real `raku`
as well as under mutsu, which is what establishes it as spec-faithful rather
than merely self-consistent.

The already-whitelisted roast files that touch this area —
`S16-filehandles/argfiles.t`, `S32-io/io-cathandle.t`, `S16-io/words.t`,
`S32-io/slurp.t`, `S02-types/WHICH.t`, `6.c/MISC/misc-6.c.t` — all still pass.
No new roast file was unblocked: the only non-whitelisted consumer,
`MISC/misc.t`, is held up by unrelated blockers (`:sym<>` colonpair
reservation on sub names, native num defaults, `undefine` deprecation) and
`raku` itself aborts partway through it.
