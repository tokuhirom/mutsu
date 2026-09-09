# Thread clones share the interpreter's program tables instead of copying them

Spawning used to cost more the bigger your program was, for no reason connected
to the work being spawned.

`clone_for_thread_excluding` builds the `Interpreter` a spawned thread runs on,
and it deep-copied about fifty of its symbol tables to do it: `loaded_modules`,
`exported_subs` / `exported_vars` / `module_owned_exports`, `type_metadata`,
`package_lexicals` / `unit_lexicals` / `module_scope_lexicals`,
`need_hidden_classes`, `class_declaring_units`, `package_distributions`,
`native_call_specs` and the rest. Every `String` key in every one of them, on
every clone. With the Cro/OpenSSL/CBOR stack loaded that came to 62 hash-table
copies and roughly 13 million instructions per spawn.

A thread clone is not a rare event. `start` makes one, `.then` makes one, and so
does every `whenever` registered on a `Promise` -- which is what
`Cro::HTTP2::GeneralParser` does once per HTTP/2 stream, for the per-stream
cancellation promise.

## The measurement that named it

The same six lines of Raku, timed in two programs:

```raku
my $p = Promise(supply {
    my $joined = Buf.new;
    whenever $sup.Supply -> $blob { $joined.append($blob); LAST emit $joined }
});
$p.result;
```

| | per call |
| --- | --- |
| bare program | 0.31 ms |
| after `use Cro::HTTP2::RequestParser` | 1.59 ms |

Identical code, 5x the cost, purely because there were more modules whose tables
had to be copied. That block is not a benchmark: it is
`Cro::MessageWithBody.body-blob`, the method an HTTP body check goes through.

## What changed

Those fields are `std::sync::Arc<...>` now. Reads are unchanged -- they go
through `Deref` -- and every write goes through the new
`runtime::cow_table_mut`, an `Arc::make_mut` wrapper that counts the copies it
causes. So a spawn shares the tables (a handful of refcount bumps) and only a
*write* taken while a clone still holds the share copies the one table it
touches.

The semantics are identical: a thread clone that writes one of these tables
still gets its own copy of it, so its declarations do not leak back to the
parent. Only the *timing* of the copy moved -- from every spawn, to the first
write after a spawn.

The counter immediately earned its keep. It showed one table still being copied
per HEADERS frame, from `writeback_package_scope_var`: that function took a
mutable borrow of `package_lexicals` to *look up* whether the name was there at
all, and the lookup missed every time. It probes read-only first now.

## Results

HTTP/2 request parser, release build, this container:

| | before | after |
| --- | --- | --- |
| HEADERS frame | 23.4 ms | 15.4 ms |
| `body-blob.result` per request | 6.0 ms | 2.5 ms |
| `program-table-cow: clones` over 40 frames | n/a | 0 |

And on the motivating case -- `cro-http`'s `t/http2-request-parser.rakutest`
under `MUTSU_REAL_TEST=1`, the vendored-`Test` gate regression tracked as item 3
of [#7555](https://github.com/tokuhirom/mutsu/issues/7555) -- the test went from
never passing to mostly passing:

| | passes out of 20 runs |
| --- | --- |
| before | 0 |
| after the registry-COW fix alone | 3 |
| after this change | 15 |

That test loses a race whose two sides start at the same instant: one waits for
the DATA frame and then reads `*.body-blob.result`, the other runs three `ok`
assertions. `body-blob` was the dominant term on the losing side, which is why
this change moves the outcome and a uniformly faster interpreter would not have.
It is not reliable yet -- the remaining margin is about a millisecond -- so
#7667 stays open.

`tests/program_tables_shared_across_thread_clones.rs` pins the counter against
frame count; `t/thread-clone-program-table-isolation.t` pins the isolation
semantics the sharing must not disturb.
