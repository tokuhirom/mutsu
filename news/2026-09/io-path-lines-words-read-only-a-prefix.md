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

Release build, five calls each (the benchmark in the issue):

| case | before, 4 MB | before, 8 MB | after, 4 MB | after, 8 MB |
| --- | ---: | ---: | ---: | ---: |
| `IO.lines.head(3)` | 0.82 s | 1.41 s | 0.001 s | 0.0003 s |
| `IO.words.head(3)` | 1.41 s | 3.22 s | 0.0002 s | 0.0002 s |
| `IO.lines.elems` | 0.53 s | 1.14 s | 0.63 s | 1.22 s |
| `IO.words.elems` | 0.77 s | 1.79 s | 0.73 s | 1.72 s |

Pinned by `t/io/io-path-lines-words-lazy.t`, which also checks that 200 rounds of prefix reads
leave no descriptors open.
