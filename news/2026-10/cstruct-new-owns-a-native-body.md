# A struct built in Raku owns a native body

`CStruct.new`, `.bless` and `nqp::create` now allocate real C memory
(#11209, #11753, [ADR-11209](../../docs/adr/11209-cstruct-new-allocates-native-storage.md)).
A class declared `is repr('CStruct')` used to be an ordinary instance when Raku
built it: it reported `P6opaque`, could not be cast to a `Pointer`, and reached
a native routine as NULL, so `memcpy($dst, $src, nativesizeof(Foo))` between two
structs made with `.new` crashed the process. An object now owns a zeroed,
aligned block of `nativesizeof(Foo)` bytes, freed together with the object, and
its address is the address a handle C returned would carry. That means:

- `.REPR` is `CStruct`, `nativecast(Pointer, $s)` is the block, and the struct
  is passed to C as a real pointer. What C writes into it reads back, even for a
  field Raku had set first;
- named arguments, declared defaults and a `BUILD`/`TWEAK` that assigns are
  migrated into the block; `$!x = v`, `$!x := $y` and an `is rw` accessor write
  through to it;
- `HAS` members and inline `HAS T @.x[N] is CArray` arrays live in it;
- a `Str`, nested-struct, `CArray` or `Pointer` field keeps what it points at
  alive for the struct's life (the `Str` gets a copy the struct owns), and reads
  back as the very object it was bound to while it still points at it;
- `.gist` and `.raku` print the live field values, for a handle C returned as
  well (it printed `Rec.new`).

`CUnion` and `CPPStruct` ride on the same body. A union's members all start at
offset 0 and its size is its largest member, so a float member, a later write and
passing it to C work (the old integer-only byte-overlay constructor is gone);
`CPPStruct` has a REPR, a layout and `nativesizeof` (it died on a `P6opaque`).

A struct class with no fields, or with a field NativeCall cannot lay out, has no
storage. Rakudo refuses to compose such a class; mutsu still does, so passing
one to a native routine now raises a catchable error instead of handing the
callee NULL.

`t/nativecall/nativecall-repr-body.t` used to pin the `P6opaque` answer for a
Raku-built struct as a safety measure; with a body behind it that reason is gone
and the test now expects Rakudo's `CStruct`. The upstream trial's CStruct step
passes a Raku-built struct through `memcpy`. Found on the way: #12105 and #12106.
