# `Promise.cause`'s backtrace duplicates the `in block <unit>` frame

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/concurrency.rakudoc:124`).

## Repro

```raku
my $promise = Promise.start({ die "Broken Promise" });
try $promise.result;
say $promise.cause;
```

- raku:
  ```
  Broken Promise
    in block  at ... line 1
  ```
  (a single backtrace frame, and the anonymous block inside `Promise.start` shows an unnamed
  `in block ` rather than `<unit>`)
- mutsu (`target/debug/mutsu`):
  ```
  Broken Promise
    in block <unit> at ... line 1
    in block <unit> at ... line 1
  ```
  (the `in block <unit> at ... line 1` frame is duplicated, and both are labeled `<unit>` instead
  of raku's unnamed anonymous-block frame)

## Analysis

Minor, cosmetic backtrace-formatting bug: `.cause`'s exception backtrace for a `Promise.start`
block gets an extra duplicate frame (and mislabels the anonymous `Promise.start` block as `<unit>`
— the top-level program unit — rather than an anonymous inner block). Likely the backtrace
construction path for a Promise's stored exception walks/appends a frame twice, or reuses the
outer `<unit>` frame's label for the inner anonymous-block frame instead of a distinct one.

## Affected files (starting point)

- Wherever `Promise.start`'s worker-thread exception capture builds/formats the backtrace for
  later `.cause`/`.result` access (concurrency/Promise implementation, backtrace formatting code).
