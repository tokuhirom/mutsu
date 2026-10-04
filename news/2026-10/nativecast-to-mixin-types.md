# `nativecast` to `Pointer[T]` and `CArray[T]` answers that exact type

Upstream NativeCall builds `Pointer[T]` and `CArray[T]` in `^parameterize`
as mixins: `Pointer.^mixin(TypedPointer[T])`. `nqp::nativecallcast` refused a
mixin type object with "nativecast() expects a type object", and that blocked
seven `t/` files on the #11203 switch to the vendored module.

A cast to such a type now answers an object of exactly that type over the C
address, as MoarVM's CPointer and CArray REPRs do:

- A CPointer base gives a pointer holding the address, carrying the type's
  roles (`.of`, `.deref`).
- A CArray base gives an **unmanaged** CArray: a view whose elements are the
  C memory. `nqp::atposref_*`, `atpos_*` and `bindpos_*` read and write that
  memory in place, and nothing is freed with the view. Like MoarVM's, it has
  no length, so `nqp::elems` on it dies with "Don't know how many elements a
  C array returned from a library".

The same boxing now serves `nqp::box_i($addr, $type)` and a `cpointer` return
of `nqp::nativecall($type, ...)`. Before, a mixin type boxed as a plain `Int`,
and a pointer return was the native provider's own `Pointer`, so the return
type's methods were missing ("No such method 'Int' for invocant of type
'Any'").
