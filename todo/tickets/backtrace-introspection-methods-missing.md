# `Backtrace` is missing several introspection methods (`next-interesting-index`, `outer-caller-idx`, `nice`)

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Backtrace.rakudoc:72,84,96`).

## Repro

```raku
sub zipi { { { die "Something bad happened" }() }() };
try zipi;
say $!.backtrace.next-interesting-index;           # raku: 2
say $!.backtrace.next-interesting-index( :named );  # raku: 4
say $!.backtrace.outer-caller-idx( 4 );              # raku: [6]
say $!.backtrace.nice( :oneline ) if $!;             # raku: "  in sub zipi at ... line 1"
```

- `raku`: all four calls succeed, returning the documented values.
- `mutsu` (`target/debug/mutsu`): each dies with
  `No such method '<name>' for invocant of type 'Backtrace'` — none of
  `next-interesting-index`, `outer-caller-idx`, or `nice` are implemented on `Backtrace`.

## Root cause

`Backtrace` (wherever it is implemented as a builtin type, grep for `"Backtrace"` in
`src/runtime/` and `src/builtins/`) does not define these three methods. Per
`raku-doc/doc/Type/Backtrace.rakudoc`:

- `.next-interesting-index([:$named])` — returns the index of the next "interesting"
  frame (skipping setting/internal frames) after the given/first one.
- `.outer-caller-idx($from-idx)` — returns the frame indices of the callers of a given
  frame's containing routine.
- `.nice(:$oneline)` — a human-readable rendering of the backtrace (the basis of the
  default exception `.gist`).

## Affected files (starting point)

- Wherever `Backtrace`'s existing methods (`.full`, `.concise`, `.list`, etc.) are
  implemented — add these three alongside them.

Note: this is a separate finding from
[backtrace-frame-indexing-returns-nil.md](backtrace-frame-indexing-returns-nil.md)
(positional `[N]` indexing into `Backtrace` returning `Nil`), though both point at the
same underlying gap — `Backtrace`'s introspection surface is thin compared to Rakudo's.
