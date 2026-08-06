# A Buf survives nested-index assignment, and a parametric Buf/Blob native-call param no longer marshals as NULL

Two independent fixes from `todo/tickets/typed-buf-native-interop-holes.md`
(items 3 and 4; items 1 and 2 of that ticket were re-verified and no longer
reproduce).

## `@a[0][1] = v` no longer clobbers a Buf into a plain Array

```raku
my @a;
@a[0] = Buf[uint64].allocate(2);
@a[0][1] = 42;
say @a[0].^name;  # raku: Buf[uint64]   mutsu (before): Array
```

The 2-level nested-index-assign path (`src/vm/vm_var_assign_index_named.rs`)
checks whether the slot it's about to write through already holds a
container, autovivifying a fresh Array/Hash only if not. A Buf's element
storage lives in a shared attribute cell on a `Value::Instance`, not as a raw
Array/Hash payload, so the probe didn't recognize it and clobbered it — the
same gap existed for a Buf reached through a hash key (`%h<k>[1] = v`) and
through a `:=`-bound cell, both fixed by the same helper
(`write_buf_element_if_buf_instance`), used both in the array-outer
nested-assign arm and in `assign_into_nested_container` (which also serves
the hash-outer arm and `ContainerRef` chains).

## A `Buf[T]`/`Blob[T]` native-call parameter marshals as the buffer's address, not NULL

```raku
use NativeCall;
sub strlen(Buf[uint8]) returns size_t is native { * }
say strlen(Buf[uint8].new(72, 105, 0));  # raku: 2   mutsu (before): SEGV
```

`CType::from_type_name` (`src/runtime/nativecall.rs`) only recognizes the
bare stems `Buf`/`Blob`/`buf8`/`blob8` — a parameterized spelling like
`Buf[uint8]` reached it unstripped, matched nothing, and fell through to the
"starts-uppercase ⇒ opaque `CStruct` pointer" heuristic in
`vm_register_sub_ops.rs`. A plain Buf instance has no `address` attribute for
that path to read, so NULL was passed to C — silently for a function that
merely returns a length, and with a SEGV for one like `strlen` that actually
dereferences the pointer. Fixed by stripping a `Buf[...]`/`Blob[...]`
parameter to its stem before the `CType::from_type_name` lookup, same as the
existing `CArray[T]` handling just above it.

Regression tests: `t/buf-nested-index-assign-preserves-buf.t`,
`t/nativecall-parametric-buf-param-marshals-address.t`.
