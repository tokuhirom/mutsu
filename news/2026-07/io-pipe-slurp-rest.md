# `IO::Pipe.slurp-rest`, and a cursor-aware `IO::Pipe.slurp`

`slurp-rest` — the IO::Handle method that reads a handle from its current
position to the end — was missing on `IO::Pipe`, so `run(:out, ...).out.slurp-rest`
died with "No native method 'slurp-rest' on IO::Pipe". It is a very common idiom in
distribution test suites (`.out.slurp-rest(:close)` is how a test reads a
subprocess's captured output), and every such subtest aborted before running.

Two related fixes:

1. **`slurp-rest` is now dispatched on `IO::Pipe`** — for the stdin pipe, the
   "live" out/err pipe, and the buffered pipe alike — and is registered in the
   type's native-method whitelists so introspection agrees.
2. **`slurp` / `slurp-rest` on a buffered pipe are cursor-aware.** They read from
   the pipe's current cursor to the end and leave it drained, matching Rakudo:
   `$proc.out.get` followed by `$proc.out.slurp-rest` yields the *remainder*, not
   the whole output again, and a second `slurp-rest` returns the empty string.
   `:bin` returns the remaining bytes as a `Buf`; `:close` closes the pipe.

Found while triaging `TODO_dist` ticket T-046 (RakudoPrereq), whose `xt/01-operation.rakutest`
runs nine subtests that each read a subprocess with `.slurp-rest(:close)`.

Pin: `t/io-pipe-slurp-rest.t` (passes under both mutsu and raku).
