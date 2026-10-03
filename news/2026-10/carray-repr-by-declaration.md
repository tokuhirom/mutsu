# `is repr('CArray')` allocates native storage, and upstream `CArray[int32]` works

Upstream `NativeCall::Types` declares its C array as an ordinary class,
`our class CArray is repr('CArray') is array_type(Pointer)`, and builds every typed
array in `^parameterize` by mixing in a role (`array.^mixin(IntTypedCArray[int32])`)
whose element access is nqp code on `self`: `nqp::create(self)`, `nqp::bindpos_i`,
`nqp::atposref_i`, `nqp::elems`. Three general gaps stopped it on mutsu
([#11209](https://github.com/tokuhirom/mutsu/issues/11209), part of
[ADR-11203](../../docs/adr/11203-nativecall-runs-upstream-via-the-backend-neutral-path.md)):

- **`nqp::create` dropped a mixin's roles.** `nqp::create` of a mixin type object
  created an instance of the base class, so `CArray[int32].new(1, 2, 3)` died with
  "No such method 'ASSIGN-POS'". It now composes the mixin's roles onto the new
  instance (without running their `BUILD`, as `create` runs nothing), for a mixin type
  object and for an object with roles mixed in alike.
- **The REPR was chosen by class name.** A class declared `is repr('CArray')` is now
  recorded, and `nqp::create` of it (or of a mixin of it) gives the instance native
  element storage — the same contiguous node a `Buf` uses (ADR-0015) — typed by the
  type's `.^array_type`. That answer now includes a mixed-in role's
  `is array_type(TValue)`, and a `native` type's `is nativesize` / `is ctype` /
  `is unsigned` decide the element width and signedness. As in rakudo, a REPR is not
  inherited by a subclass.
- **`nqp::atposref_*` had no container for a native element.** On native storage
  (a `Buf`, a `CArray`) it now answers MoarVM's `IntPosRef` / `UIntPosRef` /
  `NumPosRef`, a `Proxy` subclass whose FETCH decodes the live element and whose STORE
  encodes into it, so `my $r := $a[1]; $r = 42` writes into the array.

Two smaller fixes came with it: a Proxy returned from a method is now FETCHed with the
Proxy itself as the argument (it used to get a fresh attribute-less Proxy, so a Proxy
subclass's FETCH could not see its own attributes), and `.VAR.^name` of a Proxy
subclass reports the subclass (`class P is Proxy`) rather than `Proxy`.

`scripts/nativecall-upstream-trial.sh` now passes both `CArray[int32]` steps; the next
stop is `nqp::nativecallsizeof` (#11211). Reference-element arrays (`CArray[Str]`,
`CArray[Pointer]`, ADR-0015 P3c), `CStruct.new` allocation, `CUnion`, `CPPStruct`,
`CStr` and the `NativeCall` REPR remain on #11209. Pinned by
`t/nativecall/carray-repr-nqp-create.t`, `t/oo/nqp-create-mixin.t` and
`t/vm/nqp-atposref-buf.t`.
