# DBIish upstream Pg test suite: five more NativeCall/introspection fixes

Running DBIish 0.6.8's own Pg test files (11 files, live PostgreSQL 16)
after the end-to-end milestone surfaced five more general bugs. Six of the
eleven files now match raku exactly; the remainder are ledgered in
`todo/tickets/dbiish-pg-upstream-suite-parity.md` with one deep blocker split
into its own ticket (`module-loaded-sub-with-tail-var.md`).

1. **`is rw` numeric parameters are out-parameters.** C receives a `T*` and
   writes the result through it — libpq's
   `PQescapeByteaConn(..., size_t *to_length)` and
   `PQunescapeBytea(str, size_t is rw --> Pointer)`. mutsu passed the value
   itself, so the callee wrote through a garbage pointer (a segfault, when
   lucky). The slot is seeded, passed as a pointer, decoded after the call,
   and written back — through the argument's shared container cell when it
   has one, and by variable name at the VM call site when the argument
   arrived as a plain `VarRef` (including an inline
   `my size_t $elems` declared in the argument list).

2. **A definedness smiley is not part of the C type.**
   `memcpy(Blob:D $dest, ...)` (NativeHelpers::Blob) marshals as `Blob`; with
   `:D` attached the name missed every scalar mapping, fell through to the
   opaque-handle branch, and the address-less Buf became a NULL the callee
   wrote to.

3. **`ret_struct` resolves at call time too.** A native sub declared INSIDE
   the class body it returns (`sub PQconnectdbParams(... --> PGconn)` inside
   `class PGconn`) registers before the class exists, so the
   registration-time resolution left the short name and ordinary Raku
   methods on the returned handle (`PGconn.escapeBytea`) failed to dispatch.
   A unique `::Short` suffix match among registered classes recovers it.

4. **Uninitialized C-width-alias native scalars read as 0.**
   `my size_t $sz;` / `my ulong $u;` answered Nil (and died on first read);
   the parser's zero-seed lists only knew the sized spellings. Both lists now
   defer to `NATIVE_INT_TYPES`.

5. **`Buf.of` / `Buf.can` on the builtin surface.** `.of` answers the element
   type (bracket parameter or the width the short name encodes), and `.can`
   consults the declared builtin method lists — `blob-from-pointer` branches
   on `$type.can('allocate')` and took a REPR-poking fallback when it wrongly
   answered false.

Pins: `t/nativecall-rw-numeric-out-param.t`, `t/buf-of-and-can.t` (both
verified against raku first). The DBIish-side bugs found in the same sweep are
recorded in `docs/batteries/dbiish-upstream-bugs.md`.
