# Element assignment through an `is rw` parameter no longer replaces a Buf with a fresh Array

`sub f($d is rw) { $d[0] = 3 }` called with a `Buf` argument replaced the
caller's Buf with a plain Array holding only the assigned element,
instead of mutating the Buf in place:

```
sub f($d is rw) { $d[0] = 3; }
my $b = Buf.new(0x80, 1);
f($b);
say $b.raku;   # mutsu (before): [3]     raku: Buf.new(3,1)
```

A `Buf $d is rw`-typed parameter (the shape Cro's
`Cro::HTTP2::FrameParser` uses) failed outright instead: "Type check
failed for an element of $data; expected Buf but got Int".

## Root cause

The Buf/Blob element-assignment lane in
`exec_index_assign_expr_named_op_inner`
(`src/vm/vm_var_assign_index_named.rs`) classifies its target by matching
`self.env().get(&var_name)` against `ValueView::Instance { .. }`
directly. An `is rw` scalar parameter binds through a `ContainerRef`
cell instead of the raw Instance, so this match missed it and fell
through to the generic array-autoviv lane, which builds a fresh Array
sized to the index — hence `[3]` (or, with a `Buf` type constraint on
the parameter, the writeback type check rejected the resulting Array's
element value against the declared type).

## Fix

Deref one level before classification: if the target resolves to a
`ContainerRef` whose held value is an `Instance`, use that Instance for
the Buf/Blob classification instead. No cell rebind is needed — a Buf's
element storage node is itself Gc-shared (`with_buf_elems_mut`/
`put_bytes` in `src/value/value_buf.rs`), so mutating through the
deref'd Instance's (cheaply cloned) attributes is visible to every alias
of the same Buf, including the caller's original binding.

## Verification

- The original repro, the `Buf $d is rw`-typed Cro shape, a non-zero
  index, and the compound `+&=` case (Cro's actual `payload()` pattern)
  all now match raku exactly. A control case (`Array $d is rw`) is
  unaffected.
- `t/http2-frame-serializer.rakutest` (vendored Cro::HTTP2 suite):
  subtest 11 ("Simple priority frame is parsed back") now passes.
- New pin: `t/rw-param-buf-element-assign.t` (passes under both `mutsu`
  and `raku`).
- Whitelisted `S03-operators/buf.t`, `S32-container/buf.t`, and the
  index/subscript/assign roast sweep (16 files, 935 subtests) pass with
  no regressions. Full `make test` passes.
