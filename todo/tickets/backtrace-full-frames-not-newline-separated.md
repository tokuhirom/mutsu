# `.backtrace.full` joins its frame lines without newlines

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/Exception.rakudoc:78`).

## Root cause hypothesis

`Backtrace.full` should render one `  in ... at FILE line N` line per frame, joined by
newlines. mutsu's implementation appears to build the frame strings correctly (each
individual frame's text matches raku's, modulo the missing `SETTING::`-internal
`die`/`throw` frames mutsu doesn't model) but concatenates them without inserting a
`\n` between frames, so the whole backtrace prints as one long line.

## Minimal repro

```raku
sub f() { die 'Bad' };
sub g() { f; CATCH { default { .rethrow } } };
g;
CATCH { default { say .backtrace.full } };
```

- `raku` (5 lines, one frame per line):
  ```
    in method throw at SETTING::src/core.c/Exception.rakumod line 65
    in sub die at SETTING::src/core.c/control.rakumod line 253
    in sub f at FILE line 1
    in sub g at FILE line 2
    in block <unit> at FILE line 3
  ```
- `mutsu` (`target/debug/mutsu`): a single line —
  ```
    in sub f at FILE line 2  in sub g at FILE line 3  in block <unit> at FILE line 4
  ```
  (also missing the two `SETTING::`-internal frames for `die`/`.throw`, which mutsu
  doesn't model as call frames at all — that part is likely out of scope / a separate,
  lower-priority gap, since mutsu's `die`/exception-throw path isn't itself
  implemented as Raku-level setting subs the way Rakudo's is).

## Affected files (starting point)

- Wherever `Backtrace.full`/`.gist` (or the underlying frame-list-to-string join) is
  implemented — grep for where backtrace frame strings (`"  in ... at ... line ..."`)
  are assembled and joined; the join should use `"\n"` between frames, matching raku.
