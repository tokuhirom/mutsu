# `Pointer[T]` — a pointer that knows what it points at

`Pointer[T]` did not exist. Parameterising it raised `Pointer cannot be
parameterized`, and neither `.of` nor `.deref` was implemented, so the two things
NativeCall bindings do with a typed pointer were both unreachable:

```raku
my \t = ptr.of ~~ void ?? $type.of !! ptr.of;         # NativeHelpers::Blob
nativecast(Pointer[type], OBJECT_BODY(any)).deref     # MoarVM::Guts::REPRs
```

## What landed

- **`Pointer[T]` parameterises.** The resulting object stays an ordinary
  `Pointer` and remembers `T` in an `of` attribute, rather than becoming an
  instance of a class named `Pointer[T]` — every existing `Pointer` method
  (`.Int`, `.gist`, and the marshalling layer's `address` read) keeps working
  unchanged.
- **`.of`** reports that type, or `void` for an untyped pointer. `void` is a new
  prelude type: it exists only to be compared against, which is all Rakudo's
  `NativeCall::Types::void` is used for here.
- **`.deref`** reads through the pointer. A pointer to a struct yields a handle
  onto that same address — C holds structs by reference, so
  `nativecast(Pointer[SomeStruct], $p).deref.field` reads (and now writes) the
  struct in place; a pointer to a native scalar reads the value there, which is
  element 0 of the equivalent `CArray[T]`. An untyped pointer refuses, as Rakudo
  does.

## The bug behind the bug

A **`Pointer[T]`-typed field** was not recognised as a pointer, and a field type
NativeCall cannot marshal *aborts the whole struct layout* — deliberately, since
continuing past it would give every later field a wrong offset. So a struct
carrying a single `has Pointer[my_bool] $.error;` had no layout at all, and every
field access on it failed along with `nativesizeof`. `DBIish`'s `MYSQL_BIND` is
exactly that shape:

```raku
class MYSQL_BIND is repr('CStruct') is export {
    has intptr           $.length is rw;
    ...
    has Pointer[my_bool] $.error;
    has Pointer[uint8]   $.row_ptr;
```

`nativesizeof(WithTyped)` answered `16` in raku and died in mutsu. It answers 16
now.

While fixing it: a **NULL `Pointer`-typed field now reads as a defined `Pointer`
with address 0**, not as a type object. The type-object rule is right for a
CStruct handle (Rakudo returns a type object for a null struct, so `.defined`
works), but `Pointer.new(0)` is a defined value in Rakudo too, and the old
behaviour made `$s.field.Int` come back empty instead of `0`.

This is [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)'s
**P1a** — the half of P1 that needs no `.REPR`/`.WHERE` work and so carries none
of the ordering hazard. Pinned by `t/nativecall-typed-pointer.t`, 14/14 identical
under `raku`.

One finding recorded rather than fixed: a CStruct field whose name collides with
a builtin method (`first`, `elems`, …) is unreachable, because a handle's fields
are reached from the accessor *fallback*, after builtin dispatch. Not currently
blocking any binding —
[`todo/tickets/cstruct-field-shadowed-by-builtin-method.md`](../../todo/tickets/cstruct-field-shadowed-by-builtin-method.md).
