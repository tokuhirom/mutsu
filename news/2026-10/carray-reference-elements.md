# Reference-element `CArray` storage, selected by `is repr`

A class declared `is repr('CArray')` whose element type is a reference --
`Str`, a `CPointer`/`CStruct` class, a nested `CArray` -- now has real C
storage (#11209, ADR-0015 P3c). As in MoarVM's CArray REPR it keeps two
tables: the addresses C sees (`char**`, `void**`), held in the same native
buffer node a numeric `CArray` uses so that a native call is handed the array
itself, and a child table with the object each slot stands for. A `Str` slot
owns its NUL-terminated copy for as long as the array references it. When C
rewrites a slot (`strtol`'s `endptr`), the next read materialises a fresh
object at the new address instead of answering the stale one.

This is the storage upstream `NativeCall::Types`' `TypedCArray` role works
on through `nqp::atpos`/`nqp::bindpos`, so `CArray[Str].new("ab", "cd")` from
the vendored module now builds and reads back.

Several general gaps were fixed along the way, each hit by that module:

- `.new` and `bless` allocate `CArray` storage the way `nqp::create` does.
- `$x[$i] = $v` on an object with roles mixed in (`C.new but R`, an instance
  of a `.^mixin` type) dispatches the role's `ASSIGN-POS`/`ASSIGN-KEY`. It
  used to replace `$x` with a fresh `Array`.
- A type constraint `C[T]` for a class with its own `method ^parameterize`
  is checked against the type that meta-method builds, so
  `sub f(CArray[Str] $a)` accepts a `CArray[Str]`; such a class is no longer
  rejected as `X::NotParametric` at compile time.
- Smartmatching against a mixin type compares parameterized roles with their
  arguments: `C.^mixin(R[Str]).new ~~ C.^mixin(R[Int])` is now `False`.
- `nqp::box_i($addr, $T)` boxes into any `is repr('CPointer')` class.
- A `CArray` argument that is a mixin instance is passed to C from its own
  storage; it used to be handed a dangling empty buffer.
