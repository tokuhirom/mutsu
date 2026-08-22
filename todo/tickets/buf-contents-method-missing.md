# `Buf`/`Blob.contents` method is missing

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/experimental.rakudoc:32`).

## Root cause

`Blob`/`Buf` has a `.contents` method that returns the list of byte values (equivalent to
`.list`/iterating the buffer as integers). This is a general `Blob`/`Buf` method, not
specific to the `use experimental :pack` feature the doc example happens to demonstrate it
with. mutsu doesn't implement it.

## Minimal repro

```raku
my $b = Buf.new(1,2,3);
say $b.contents;       # (1 2 3)
say $b.contents.WHAT;  # (List)
```

- `raku`: `(1 2 3)` then `(List)`
- `mutsu`: `No such method 'contents' for invocant of type 'Buf'`

Doc's original example (using `pack`, unrelated to the missing method):

```raku
use experimental :pack;
say pack("H*", "414243").contents;  # OUTPUT: «(65 66 67)␤»
```

## Affected files (starting point)

- `src/builtins/methods_0arg/` — wherever other `Buf`/`Blob` 0-arg methods (`.list`,
  `.elems`, `.bytes`, etc.) are implemented; `.contents` should be a straightforward
  addition alongside those, returning the byte values as a `List`.
