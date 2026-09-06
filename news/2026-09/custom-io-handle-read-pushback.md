# A custom handle's over-returning `READ` is buffered, and the last IO::Handle row closes

`todo/deep/custom-io-handle-write-read-not-dispatched.md` opened as "custom
`IO::Handle` subclasses overriding `WRITE`/`READ`/`EOF` are not honored by
print/say/read — this is completely unimplemented in mutsu". It was not: the
routing existed (`try_user_io_handle_method`,
`vm/vm_call_method_compiled_io.rs`) and was merely wired into two of the
interpreter's four method-dispatch entry points, which
`news/2026-09/custom-io-handle-routing-reaches-every-dispatch-entry.md` fixed on
2026-09-06. This closes the two rows that survived it.

## 1. A `READ` that over-returns is now buffered

`IO::Handle.read($n)` keeps whatever the handle's `READ` hands back **beyond**
`$n` and serves the next read from it. `Type/IO/Handle.rakudoc`'s second
"Creating Custom Handles" example depends on it: its `READ` ignores the byte
count and returns the whole buffer every time, and rakudo still prints `one`
then `two`.

mutsu returned whatever `READ` gave, so the first `.get` swallowed every line at
once — and `read_user_io_char`, which asks for **one byte at a time**, could not
work against such a handle at all.

`Interpreter::user_io_read_buffers` is now a per-handle pushback buffer, keyed
by the handle instance's id. `call_user_io_read(target, n)` serves from it,
calls `READ` only for the shortfall, keeps the excess, and hands back at most
`n` bytes. `call_user_io_eof` reports end of input only once the user's own
`EOF` says so **and** the buffer is drained — an over-returning `READ`
typically reports `EOF` immediately, having handed everything over in one call,
so consulting the user method alone would have thrown away the bytes it just
gave us.

Measured byte-identical to rakudo across `.get`, `.lines`, `.slurp`,
`.read($n)`, `.getc` and successive `.get` calls walking the pushback to `Nil`.
A well-behaved `READ` that honours its count and advances a position is
unaffected (`t/custom-io-handle-write-read.t` unchanged and green).

## 2. The "any class with a `print` method breaks an unrelated redirect" row is stale

The ticket recorded a second, separate bug: declaring *any* class with a `print`
method made an unrelated class's `$*OUT = $handle` redirect fall through to the
real stdout, because `write_to_named_handle` took its real-fd branch. Re-measured
2026-09-07 on a fresh build, with and without the extra class declaration: both
answer `[one\n]`, matching rakudo. It no longer reproduces, so no ticket is
split off for it.

## Gates

`t/custom-io-handle-read-pushback.t` (9 rows, verified green under `raku` too —
including that `.lines` on a custom handle does **not** chomp, which is rakudo's
behaviour and caught an over-confident expectation of mine).
`t/custom-io-handle-write-read.t` unchanged and green.
