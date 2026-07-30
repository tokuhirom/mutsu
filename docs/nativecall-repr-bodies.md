# NativeCall REPR bodies — the compatibility surface

This document describes what mutsu promises to a Raku module that reads an
object's **REPR body** — the layout MoarVM keeps behind each of its
representations. It is the compatibility surface
[ADR-0015](adr/0015-native-backed-container-storage-and-repr-bodies.md) §4
commits to, written down so a module author can rely on it and so a future
representation change knows what it must keep.

The pin is [`t/nativecall-repr-body.t`](../t/nativecall-repr-body.t), which
declares the body structs exactly as `MoarVM::Guts::REPRs` does and reads
through them exactly as `BODY_OF` does.

## Why anything outside mutsu cares

`NativeHelpers::Blob` — a hard dependency of the `DBIish` database battery and
of fifteen further distributions in the fez index — exists to hand a C function
the address of a Raku container's elements. It gets there by reading MoarVM's
guts:

```raku
sub OBJECT_BODY(Mu \any) { Pointer.new(any.WHERE + Offset) }
sub BODY_OF(Mu \any) {
    my \type = %known-bodies{any.REPR};      # VMArray / CArray / CStruct
    die "Can only handle …" if type ~~ Nil;
    nativecast(Pointer[type], OBJECT_BODY(any)).deref;
}
```

Two things follow, and they are the whole contract:

1. **`.REPR` is a promise.** The moment it answers `VMArray`, a module will
   *dereference* whatever `.WHERE` returned. An honest name with no body behind
   it is not a small inaccuracy; it is a segfault. mutsu therefore under-reports
   (`P6opaque`) for anything it has not built a body for, which makes `BODY_OF`
   refuse loudly instead.
2. **`.WHERE` must point at the body**, because `Offset` is a probe, not a
   constant (see below).

## The four contracts

### 1. `Offset` is 0 — `.WHERE` points straight at the body

mutsu has no object header. `MoarVM::Guts::REPRs` derives `Offset` by building a
`Pointer` over a known sentinel, reading its `.WHERE` as an array of machine
words, and scanning for the sentinel; against mutsu it finds it in word 0, so
`Offset` computes as 0 and the body is read at `.WHERE` itself.

This is pinned by [`t/nativecall-sizeof-pointer-where.t`](../t/nativecall-sizeof-pointer-where.t)
and by the `body-offset` helper in the REPR-body test, which scans rather than
assuming — the same way the module does.

### 2. The body structs mirror MoarVM's layout

mutsu commits to the *layout*, not to these being its internals. They are
synthesised on demand.

| REPR | body | mutsu's answer |
| --- | --- | --- |
| `VMArray` | `{u64 elems; u64 start; u64 ssize; void* any}` | a `Buf`/`Blob` with element storage |
| `CArray` | `{void* storage; void** child; i32 managed; i32 allocated; i32 elems}` | a `nativecast`ed `CArray` handle, and a native-backed `CArray[T]` |
| `CStruct` | `{void* cstruct; void** child_objs}` | a `nativecast`ed CStruct/CUnion handle |

`start` is **always 0**: mutsu's element storage never has an unused prefix, so
`realstart == any` and the module's `+$!start` branch is unreachable.

For the two handle kinds the body is a zero-filled block whose first word is the
handle's address — byte-identical to both layouts, since every later word of an
unmanaged cast is zero (`managed`, `allocated` and `elems` are all 0, which is
exactly what an unmanaged `CArray` handle *is*). For a `Buf`, and for a
`CArray[T]` mutsu allocated itself, the block is real and per-object; see below.

A **native-backed `CArray[T]`** — one built in Raku over a native numeric element
type — fills its `CArrayB` for real: `storage` is its element bytes, `elems` and
`allocated` its length and capacity, `managed` is 1 (owning its memory is what
having storage *means*), and `child` is always NULL, because the element types that
would need a child table keep the boxed representation (see the last section).

### 3. `.WHERE` is stable for the object's lifetime

A buffer's body block is allocated once, the first time anything asks for its
`.WHERE`, and lives in the buffer's storage node
(`src/value/value_buf_repr.rs`). Later reads refresh its four words in place;
the block itself never moves. So a C structure that captured the address — the
`MYSQL_BIND.buffer` field in `DBDish::mysql`, say — keeps reading a live element
pointer even after the buffer has been reallocated underneath it.

The block dies with the buffer. Nothing is leaked and nothing is memoised across
objects.

### 4. A pointer from `pointer-to` / `BODY_OF` is valid until the container is
resized or dies

This is exactly Rakudo's contract, no more. MoarVM's `VMArray` reallocates too,
so a `pointer-to` there is equally invalid after a resize; matching that is
parity, and promising more would mean holding dead blocks alive for a guarantee
no distribution asks for.

Within that, mutsu is deliberately generous in one direction: an ordinary
Raku-side write to a buffer that does **not** grow it past its allocation writes
*through* the existing storage rather than replacing it, so the pointer survives.

## What a native call is handed

A `Blob`/`Buf` or native-backed `CArray[T]` argument declared as a C `void*` (or a
`T*`) is passed **its own storage address**. Nothing is copied in and nothing is
copied back:

- a callee filling an out-buffer (`SSL_read`, `mysql_stmt_fetch`) writes into
  the Raku object, so there is no sync point to get wrong — and, critically, no
  sync point is *needed* for the case that has none, where C is handed the
  address through a struct field and writes it long after the call that stored
  it returned;
- a callee that retains the pointer (`BIO_new_mem_buf` builds a memory BIO
  *over* the caller's bytes) keeps seeing live memory for as long as the Raku
  object is alive.

This replaced a per-object mirror (`runtime/nativecall_pin.rs`, deleted) that
could keep a retained pointer alive but could never observe a write it did not
mediate — and, for `CArray`, a per-call copy-in/copy-out that could only reflect a
write made *during* the call.

`nativecast(Pointer[T], $carray)` reinterprets that same element address, which is
what makes `NativeHelpers::Pointer`'s arithmetic (`.succ`/`.pred`/`.add`/`+`) walk
a Raku-built array.

## Residual unsafety

C code writing into a live buffer is outside Rust's aliasing model. This is the
same posture recorded in [ADR-0013](adr/0013-container-interior-mutability-cellvalue.md)
§1.3-2 for cross-thread container writes, and the same trust every declared
NativeCall signature already receives. What changed with ADR-0015 P2 is that it
became *visible and documented* rather than hidden behind a copy that happened
to paper over it.

## What does NOT get a body

Everything else answers `P6opaque`, on purpose (ADR-0015 §5, open question 5):
each honest answer is a promise that a body exists behind it, and that is not a
promise to make idly. In particular:

- a **CStruct constructed in Raku** (`Rec.new`) has no C storage yet;
- a Raku-side **`array[T]`** — ADR-0015's P3b;
- a **`CArray[T]` whose element type is a reference**: `CArray[Str]`,
  `CArray[Pointer]`, a nested `CArray[CArray[…]]`, a CStruct element. Their
  elements are addresses of other objects, so reading one back means materialising
  the object it points at, which contiguous bytes alone cannot express — MoarVM
  keeps its parallel `child` table for exactly this. They keep the boxed
  representation and the per-call `char**` build (ADR-0015's P3c);
- an ordinary class, which also keeps its identity-derived `.WHERE`.
