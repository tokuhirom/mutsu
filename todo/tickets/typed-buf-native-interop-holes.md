# Typed (wide-element) Buf native-interop holes

Side findings from the DBIish mysql parity campaign (2026-07-29, the session
that fixed the `& 0xff` element-assign truncation and added the CArray
element-write arm). All reproduce on a plain build; none block the DBIish
suites, but each is a real divergence from Rakudo.

1. **`Buf[intptr]`-style alias fallback picks width 1.** `BufData` width is
   derived once from the class-name string (`buf_elem_width`,
   `src/value/value_buf.rs`): substring probe for "64"/"32"/"16", else 1.
   Parameterization normally resolves `constant intptr = uint64` to the class
   name `Buf[uint64]`, but if the alias ever fails to resolve, the name
   `"Buf[intptr]"` silently probes to width 1 + signed — a differently-shaped
   buffer with no error. The probe should fail loudly (or resolve through the
   registry) instead of guessing.

2. **Byte-level read/write methods use the one-byte-per-element view on wide
   Bufs.** `.write-uint64`/`.read-*` (`src/builtins/buf_write_int.rs`) go
   through `buf_bytes_or_empty`/`set_buf_bytes`; on a width-8 node
   `node_bytes` returns only the LOW byte of each element and the writeback
   stores one byte per element. `Buf[uint64].allocate(2).write-uint64(0, 8192)`
   produces an 8-element buffer with one byte of the u64 per element. These
   should address `node.bytes` directly.

3. **Nested index assignment clobbers a Buf element container.**
   `@a[0][1] = v` where `@a[0]` is a `Buf[uint64]` replaces the Buf with a
   plain Raku Array (`.^name` becomes `Array`) via the autoviv fallback in
   `src/vm/vm_var_assign_index_named.rs` (the arm that overwrites any
   container matching none of the Hash/Array/Set/Bag/Mix arms). The fallback
   should refuse to clobber an Instance carrying Buf storage or an `address`
   attribute.

4. **A `Buf[uint64]` / `Blob[uint8]` signature parameter marshals as NULL.**
   `CType::from_type_name` (`src/runtime/nativecall.rs`) only maps the bare
   stems (`Buf`, `Blob`, `buf8`, `blob8`); a parameterized spelling isn't
   stripped to its stem in `vm_register_sub_ops.rs`, hits the
   "starts-uppercase ⇒ CStruct" heuristic, becomes `CType::Pointer`, and
   `value_c_address` finds no `address` attribute — so NULL is passed to C
   with no diagnostic.

Minimal repros are one-liners per item; see
`news/2026-07/dbiish-upstream-suite-parity.md` for the campaign context.
