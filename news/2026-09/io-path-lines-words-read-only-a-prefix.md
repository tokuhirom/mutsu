# `$path.IO.lines.head(3)` reads three lines, not the whole file

`IO::Path.lines` and `.words` used to read the whole file with `fs::read_to_string`, split it
into an eager `Seq`, and only then let `.head(3)` pick three elements. Five calls on an 8 MB file
took 1.2 s for `lines` and 2.4 s for `words`, and the time doubled with the file. Rakudo's
`IO::Path.lines` is `self.open(...).lines(:close)`: it returns the handle's line iterator, so
`.head(3)` reads three lines.

mutsu now does the same (#9257). `IO::Path.lines` / `.words` open a read handle and return the
same deferred `SeqSource::IoLines` Seq that `IO::Handle.lines` returns, so `.head(n)` and `.first`
take the O(prefix) path ADR-0119 built for it. `:chomp`, `:nl-in` and `:enc` are passed to the
handle (`:enc` used to be ignored by `lines`), and a numeric `$limit` reads that many records and
closes the handle.

The handle is private to the Seq, which raised two problems.

- **File descriptors.** Rakudo leaves a half-read handle to its GC. mutsu has no handle
  finalizer, so a half-read handle would stay open for the rest of the program, and a loop of
  `.head` calls would run out of descriptors. The private handle is therefore closed at EOF, after
  a consuming `.head` / `.first` (the Seq can never be read again), and when a `for` loop that
  claimed it is left, by `last` or otherwise. A subscript leaves the Seq readable, so
  `$path.IO.lines[0]` still reads the whole file and closes the handle at EOF.
- **Full reads.** A handle's line read went to the unbuffered file one byte per `read(2)`, and
  paid a handle-table lookup, a lock and an `@*ARGS` lookup per line. The private handle reads
  through a 64 KiB buffer (`SeqFileReader`), which cuts a record by scanning the buffer in place;
  a full read of the Seq drains the file under one lock; words are decoded once and split; and
  the `@*ARGS` / `$*IN` lookups now happen only for an `ArgFiles` handle. A full `.lines.elems`
  or `.words.elems` costs what the old slurp did.

Release build, five calls each (the benchmark in the issue), before and after on the same box:

| case | before, 4 MB | before, 8 MB | after, 4 MB | after, 8 MB |
| --- | ---: | ---: | ---: | ---: |
| `IO.lines.head(3)` | 0.76 s | 1.36 s | 0.0014 s | 0.0003 s |
| `IO.words.head(3)` | 1.16 s | 2.67 s | 0.0002 s | 0.0002 s |
| `IO.lines.elems` | 0.48 s | 1.07 s | 0.65 s | 1.18 s |
| `IO.words.elems` | 0.77 s | 1.74 s | 0.82 s | 1.88 s |

The full reads are within noise of each other: ten `.lines.elems` calls on the 4 MB file,
repeated three times, took 1.03-1.06 s after against 0.99-1.03 s before.

A BOM-only file has no lines, as before: the private reader drops a UTF-8 BOM before it cuts the
first record, since each record is decoded on its own (`roast/S16-io/bom.t`).

Pinned by `t/io/io-path-lines-words-lazy.t`, which also checks that 200 rounds of prefix reads
leave no descriptors open.
