# IO::Handle: 50 more built-in methods are table rows, and the VM's handle fast paths are gone

Slice 3E (part 2) of [ADR-11276](../../docs/adr/11276-built-in-methods-are-handler-rows.md) moved
the methods of `IO::Handle` into the built-in method table: 993 rows are registered now, up from 943.
The state methods (`path`, `Str`, `gist`, `nl-out`, `chomp`, `encoding`, `tell`, `eof`, `seek`,
`close`, `flush`, `lock`, ...), the reads (`get`, `getc`, `readchars`, `lines`, `words`, `read`,
`slurp`, `split`, `comb`, `Supply`), the writes (`print`, `put`, `say`, `printf`, `print-nl`, `write`,
`spurt`) and `open` each have a row. The 800-line `match` of `native_io_handle` is one
`Interpreter::io_handle_*` method per row, and `native_io_handle` is now a lookup of the row by owner.

A handle's methods used to exist three times, and now they exist once: the VM's four per-method
fast paths (`print`/`say`, `write`/`spurt`, `get`/`slurp`/`read`, the state getters) and their ten
call sites are deleted, together with the `IoHandleState` helpers only they called. The fast paths
served File and UTF-8 targets only and declined the rest to the interpreter's copy; the row serves
every target. The user-subclass overlay (an `IO::Handle` subclass with its own `WRITE`/`READ`) is
unchanged: it is Raku's contract, not a built-in method.

Two mechanisms came with it. `RowFlags::OWNER_ONLY` registers a row for the owner lookup only,
because `$fh.open` writes the opened handle back over the receiver and only the mutating dispatch
does that (slice 3F's `Handler::Mut` retires the flag). And the "did user code wrap this?" gate of
the table now sees a `.wrap` of a built-in *instance* method: it keyed on the type name `Any` for
every instance, so `$*OUT.^find_method('print').wrap: ...` would have been skipped by the new row.

No behaviour a script could see changed. A handle `slurp` now decodes through the one text decoder
(NFC-normalized) in the interpreter's copy as well, which is what the VM's copy did.
`words(N)` after a `get` leaving a stale word across `seek(0)` was found on the way and is filed
as [#12186](https://github.com/tokuhirom/mutsu/issues/12186).
