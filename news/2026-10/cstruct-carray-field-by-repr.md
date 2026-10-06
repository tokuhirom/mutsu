# A `CArray` inside a `CStruct` is selected by its REPR

A struct field whose type is a class declared `is repr('CArray')` is now one
pointer in the struct's layout and reads back as that class over the memory it
points at (#11209, ADR-11203 §2.4). Before, `cstruct_layout.rs` recognised a
`CArray` field by the spelling `CArray` / `CArray[`: that is the native
provider's class, so with upstream NativeCall's own `CArray[T]` (a mixin type
`^parameterize` builds) a struct read back one element instead of the array,
and a class with any other name could not be a field at all (the layout failed
and `nativesizeof` reported a P6opaque).

What changed:

- A field of a `CArray`-REPR class, a `HAS T @.x[N] is CArray` inline array and a
  `CArray[T]` field read back as the declared type over the pointer (a NULL
  pointer is the type object, as in Rakudo). A `my class` works too: it is
  registered under its declaration-site storage name, and the read now resolves
  the declared name the way the layout does.
- `nativecast(SomeCArrayClass, $ptr)` boxes an unmanaged CArray by REPR for the
  bare class as well as for a `CArray[T]` mixin, and `.REPR` of such a view
  answers `CArray` whatever the class is called.
- `nqp::nativecallrefresh`'s comment now says what is true: children cached for
  reference members are handles validated by address, so a read is never stale.

`t/nativecall/nativecall-cstruct-carray-field-by-repr.t` pins it with classes
the program declares itself (so nothing in them is NativeCall's `CArray`),
against Rakudo. `scripts/nativecall-upstream-trial.sh` gains the inline-array
step. With every NativeCall `t/` file rewritten onto the vendored types, the
two CStruct tests that failed on upstream types (an inline array in the body, a
`CArray` field read after C copied the struct) pass.

The measurement also found that a parameter typed through the imported `CArray`
alias does not bind a `CArray[T]` instance (#12121); that belongs to the switch.
