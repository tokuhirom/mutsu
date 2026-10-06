# ADR-11209: A Raku-allocated `is repr('CStruct')` object owns a native body; C memory is the truth

- **Status**: Accepted (user decision 2026-10-06); implemented in [#12113](https://github.com/tokuhirom/mutsu/pull/12113), with the CArray-in-CStruct reads in [#12124](https://github.com/tokuhirom/mutsu/pull/12124)
- **Date**: 2026-10-06
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11209](https://github.com/tokuhirom/mutsu/issues/11209)
- **Related**: [ADR-11203](11203-nativecall-runs-upstream-via-the-backend-neutral-path.md) (upstream
  NativeCall verbatim), [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md)
  (native-backed storage; this is its CStruct counterpart), [ADR-0090](0090-has-embedded-cstruct-members.md)
  (`HAS` layout)

## 1. Context

A CStruct in mutsu is a **handle**: an `Instance` of the declared class whose `address`
attribute is a C pointer, with no Raku attributes of its own. Field reads (`$s.x`, `$!x`) are
resolved against `runtime::cstruct_layout` and read out of the pointed-to memory; `$s.x = v`
(an `is rw` accessor) writes there. That is exactly what a struct *C hands back* needs.

A struct **allocated from Raku** (`Foo.new(...)`, `Foo.bless(...)`, `nqp::create(Foo)`) never
got that treatment. It is an ordinary instance whose fields sit in the attribute cell, with no
`address`. Measured on main (2026-10-06), against `raku`:

| | Rakudo | mutsu |
| --- | --- | --- |
| `Foo.new(a => 1).REPR` | `CStruct` | `P6opaque` |
| `nativecast(Pointer, $s)` | the struct's address | a NULL-ish type object (`Int` dies) |
| `memcpy($dst, $src, nativesizeof(Foo))` with both structs from `.new` | copies | **SIGSEGV** (the argument reaches C as NULL) |
| a `CStruct` handed to C and filled there, then read back | the C-written fields | the fields Raku last set |

The last two are the point of the declaration. The SIGSEGV is also a memory-safety bug in the
shipped binary: a program that passes a struct it built itself to any native function crashes
the process rather than raising an exception.

Upstream NativeCall (ADR-11203) is vendored to run verbatim, and bindings in the ecosystem build
structs this way constantly (`my $ev = Event.new; poll($ev)`).

## 2. Decision

**An instance of a class declared `is repr('CStruct')` that Raku allocates owns a zeroed,
aligned block of C memory of `nativesizeof(Class)` bytes. That block, not the attribute cell,
is the object's state.**

1. **Allocation is part of construction, selected by the declaration** (not by a class name):
   `.new` and `bless` allocate after the constructor ran, like the CArray REPR step
   (`install_carray_storage`); `nqp::create` allocates a bare zeroed block with no defaults and
   no `BUILD`. Values the constructor left in the cell (named arguments, defaults, a `BUILD`'s
   `$!x = ...`) are **migrated** into the block field by field, so every user-visible
   construction path keeps working unchanged, including a `BUILD`/`TWEAK` that assigns.
   Allocation is idempotent (`new` calling `bless` allocates once).

2. **The instance becomes an ordinary handle**: the block's address is stored in the existing
   `address` attribute. Everything built for C-returned handles then applies unchanged: field
   reads, `is rw` accessor writes, `HAS` members as handles onto inline storage, passing the
   object to a `char*`/`void*`/struct parameter (a real pointer, no more NULL), `nativecast`,
   `.WHERE`, and `.REPR` answering `CStruct`.

3. **Ownership is the object's lifetime.** The block is a detached byte block (the type
   `carray_ref` already uses for its `Str` copies) held in a hidden attribute, so it is freed
   when the last alias of the instance dies, with no `Drop` impl and no leak. Alignment is
   guaranteed by over-allocating and storing the aligned address, not by trusting the allocator.
   A C library that keeps the pointer past the object's life reads freed memory, as in Rakudo.

4. **Writes go through to C memory.** `$!x = v` inside a method stores in the attribute cell as
   before and, when the instance owns a body, also writes the field's bytes (`write_field`); so do
   `$!x := $y` and an `is rw` accessor assignment. The cell stays a *cache*: method entry already
   re-reads the fields from C (`seed_cstruct_fields_for_method`), accessors read C directly, and
   `.gist` / `.raku` refresh it before rendering. The test that an instance owns a body is one
   extra probe of the attribute map the write already resolved, so ordinary classes pay nothing
   measurable. (The per-site attribute cache cannot serve such an instance: the hidden attributes
   below are undeclared ones, which the cache already refuses.)

5. **A reference-typed field keeps its child alive.** A field holding a pointer to another
   Raku-allocated object (a nested CStruct, a `CArray[T]`) records that object in a hidden
   attribute of the parent, as `carray_ref` does for its slots; a read answers the recorded child
   only while the field still holds its address (C may have rewritten it) and builds a handle
   from the address otherwise. A `Str` field points at a NUL-terminated copy the parent owns.
   The pointer and its child change together under the attribute cell's write lock, and a field
   read takes the read lock for the same step, so a reader on another thread never follows a
   pointer whose target a replacement is freeing (a Raku data race may give a wrong answer, never
   a use after free; `docs/security.md`). A bare integer is refused for a struct-, union- or
   `CArray`-typed field, as Rakudo's typed assignment refuses it, so the program cannot name an
   address that the next read through the field would dereference.

6. **The layout is memoised per registry generation** (`caches`, the `create_memo` pattern) for
   allocation, and each object carries the layout it was built with (encoded as plain values in a
   hidden attribute) for its field writes, which run under `&self` and so cannot compute one. Only
   a successful layout is recorded.

7. **A class whose layout cannot be computed** (no fields, or a field NativeCall cannot marshal)
   keeps an ordinary instance with no body. Rakudo rejects such a class at compose time; mutsu
   does not, so passing one to a native routine is **refused with a catchable error** instead of
   reaching the callee as NULL (a memory-safety hole, #11753).

8. **`CUnion` and `CPPStruct` use the same mechanism.** A union's members all start at offset 0
   and its size is its largest member (a member the constructor never set must not overwrite one
   it did); `CPPStruct` is laid out as a CStruct and reports its own REPR. This replaces the
   integer-only byte-overlay constructor `CUnion` had, which could not hold a float, take a later
   write or reach C.

## 3. Options considered

1. **Sync at the boundary (cell stays the truth).** Keep the attribute cell as the state; when
   an instance is about to cross into C, allocate a block, copy the cell into it, hand over the
   pointer, and copy back after the call. *Rejected.* No hot-path hook and no storage change, but
   it is wrong whenever C keeps the pointer: a callee that retains the struct (a registered
   callback's user data, a config struct, an `epoll_event` array) sees stale or never-updated
   bytes, and a Raku-side `$!x = v` after the call is invisible to C. It also needs a copy-back
   at every one of the call paths (native call, callback, `nativecast`, `.WHERE`), which is the
   "a call path can miss it" failure `carray_ref` was designed to avoid.
2. **Leak the block** (the `CStr` approach). *Rejected for a struct.* A string literal is
   allocated a bounded number of times; a struct is allocated in loops, so a leak would grow
   without bound.
3. **A Raku-visible attribute container backed by C memory** (a `Proxy` per field). *Rejected.*
   It would make `$!x` reads and writes correct with no hook, but each field becomes a `Proxy`
   cell on every instance, `Proxy` fetch/store are Raku code objects (an interpreter re-entry
   per field access), and it still would not give C the pointer.

## 4. Consequences

- A struct built in Raku can be passed to C, cast, and read back; the SIGSEGV above becomes a
  correct call.
- `.REPR` of a Raku-built CStruct is now `CStruct`, which `t/nativecall/nativecall-repr-body.t`
  pinned as `P6opaque` as a *safety* measure ("answering the honest name without a body would
  make `BODY_OF` dereference whatever `.WHERE` returned"). With a body that reason is gone; the test
  is changed to Rakudo's answer.
- Each `.new` of a CStruct class costs one small allocation and one layout-memo probe.
- `$obj.clone` of a CStruct (Rakudo: "cloning a CStruct is NYI") keeps aliasing the same body.
- Out of scope: a reference-element view over C memory (`nativecast(CArray[Pointer], ...)`), the
  MOP's `bindattr_*`/`getattr_*` on a body-owning instance (they touch the cell only), and
  `.CREATE` called as a method (`nqp::create` is covered). A NULL `Pointer` field reading as a
  defined `Pointer` (Rakudo: the type object) is an existing divergence, filed as #12106.

## 5. Implementation status

| Slice | State |
| --- | --- |
| 1. allocation, migration, REPR, field read/write, write-through, `HAS` | Done (`src/runtime/cstruct_body.rs`) |
| 2. children of reference fields | Done (hidden `__mutsu_cstruct_child_<field>` attributes) |
| 3. `CUnion` / `CPPStruct` storage and `nativesizeof` | Done (`cstruct_class_name` covers unions; the byte-overlay constructor is gone) |
| bodyless struct argument refused | Done (`nativecall.rs`, #11753) |
