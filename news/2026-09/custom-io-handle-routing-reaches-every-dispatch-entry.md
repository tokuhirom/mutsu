# A custom `IO::Handle`'s WRITE/READ routing now reaches every dispatch entry

`Type/IO/Handle.rakudoc`'s "Creating Custom Handles" (6.d) documents that a class
`is IO::Handle` implementing `.WRITE` / `.READ` / `.EOF` gets the high-level text
methods for free. Both of the doc's worked examples failed:

```raku
my $store = IO::Store.new;
my $out = $*OUT;
$*OUT = $store;
.say for <one two three>;
$*OUT = $out;
say $store.lines;      # raku: [one\n two\n three\n]
                       # mutsu: printed to the real stdout, captured nothing
```

and the second example died outright with `Expected IO::Handle`.

## The routing already existed

`todo/deep/custom-io-handle-write-read-not-dispatched.md` called the feature
"completely unimplemented" and scoped a fix across `native_io/io_handle.rs`,
`handle_open.rs` and the say/print VM ops. None of that was needed.
`try_user_io_handle_method` (`vm/vm_call_method_compiled_io.rs`) already
implemented the whole routing — it was simply **wired into two of the
interpreter's four method-dispatch entry points**:

- the documented `$*OUT = $store` idiom reaches the handle through an *internal*
  `.print` dispatch from `write_to_named_handle`, i.e. `call_method_with_values`,
  which had no hook — so the call errored, and `write_to_named_handle`'s own
  fallback swallowed the error and printed to the real stdout;
- the read-side methods (`.read`, `.eof`, `.getc`, `.get`, `.lines`, …) are
  mut-path methods and arrive at `call_method_mut_with_values`, which had no hook
  either — so they reached the native `IO::Handle` arm and died for want of a
  real file descriptor.

`$store.print("x")` written directly in Raku worked the whole time, which is what
kept the two paths' disagreement invisible.

A third defect sat inside the routing itself: the write dispatch's `match method`
ended in `_ => return None`, so a handle overriding **both** `WRITE` and `READ`
bailed out of the function before reaching its own read block. Every read method
on such a handle was unreachable by construction — and overriding both is exactly
what the doc's second example does.

## The fix

Three small changes: the hook added to the two missing dispatch entries, and the
write match's catch-all changed to fall through to the read block instead of
returning.

`t/custom-io-handle-write-read.t` pins six rows against raku v2026.07: the
documented `$*OUT` redirect for `say` / `print` / `.say`, direct
`print`/`say`/`put` reaching `WRITE`, and `.read` / `.eof` / `.getc` reaching
`READ`/`EOF` — including that `.read` advances.

## What is left

Recorded in the (retained, narrowed) ticket:

- **A `READ` that over-returns is not buffered.** Raku keeps what `READ` hands
  back beyond the requested count and serves the next read from it, which is why
  the doc's second example — whose `READ` ignores its count and returns the whole
  buffer — prints `one` then `two` there. mutsu returns whatever `READ` gave. A
  well-behaved `READ` that honours its count works correctly today.
- **A separate bug found while testing, not about custom handles at all**:
  declaring *any* class with a `print` method makes an *unrelated* class's
  `$*OUT = $handle` redirect fall through to the real stdout. Bisected — the same
  block passes alone and fails as soon as such a class exists anywhere in the
  file, because `write_to_named_handle` then takes its real-fd branch for the
  custom handle. That is the shape of a name-keyed "a user overrode this native
  method" check that is not scoped to the receiver's class.

The methodological note worth keeping: the ticket's estimate was "a systemic
change across three subsystems". The actual defect was a hook wired into half the
dispatch entry points and one early `return` — found by asking *which* dispatch
path the failing call took (`rust-gdb` backtrace at the error site) rather than by
reading the feature's implementation.
