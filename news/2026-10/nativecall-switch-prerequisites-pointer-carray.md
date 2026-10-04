# More of upstream NativeCall's types work: Pointer fields, `Pointer[T]` declarations, `allocate(0)`

Measured on the switch branch, where `use NativeCall` loads the vendored
upstream module (#11203). These fixes take the `t/` NativeCall files from 82 to
86 of 111. Each one is a general interpreter fix:

- A CStruct field `has Pointer[int8] $.err` and `cglobal(..., Pointer)` box
  into the pointer type in scope: upstream's CPointer class, or the
  `Pointer[T]` mixin `^parameterize` built. A NULL field reads as the type
  object, as in rakudo. Before this, mutsu built the native provider's
  `Pointer` by name.
- `nqp::nativecallcast` to an `is repr('Uninstantiable')` type dies, as
  MoarVM's does. Upstream's `Pointer.deref` relies on that for an untyped
  pointer.
- `my C[T] $x .= new` and an uninitialized `my C[T] $x` use the type object
  that `C`'s own `^parameterize` built, for example upstream's
  `Pointer[uint16]`.
- `nqp::bindpos*` at a negative index of an `is repr('CArray')` array is
  dropped. Upstream's `allocate(0)` binds index `-1` of an empty array; MoarVM
  writes before its buffer there.
- `::?CLASS` in a role method dispatched on a mixin is the mixin type
  (`K+{R[Str]}`), not the role, as in rakudo.
- Type aliases in *method* signatures resolve at declaration, as #11555 did
  for subs.

Found on the way: #11804 (a sigilless capture in an anonymous role's method
reads the wrong value) and #11805 (`.^set_name` on a mixin type is not seen by
its instances).
