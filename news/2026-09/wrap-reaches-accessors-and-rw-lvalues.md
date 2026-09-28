# `.wrap` now guards rw-method assignment and auto-accessors

The Object::Permission distribution adds an `is authorised-by('perm')` trait.
The trait wraps a method, or the accessor of a public attribute, in a wrapper
that throws `X::NotAuthorised` unless `$*AUTH-USER` holds the permission. Under
mutsu, every wrapper that should have refused let the call through. Three gaps
caused this:

- **Assignment bypassed the wrapper.** `$obj.m = $v` on a `.wrap`ped
  `method m() is rw { $!x }` saw the body's `$!x` tail and stored into the
  attribute directly. The wrapper chain never ran, and neither did the refusal
  it raises. A wrapped declared method now runs its chain in lvalue mode, and
  the assignment writes through the container that the chain returns. If the
  chain throws, the exception propagates.
- **An accessor's Method object could not be wrapped.** The objects that
  `.^find_method('x')` and `.^lookup('x')` return for a `has $.x` accessor had
  no wrap identity, so `.wrap` died with `No such method 'wrap'`. The objects
  from `.^can('x')` (a bare Routine for an accessor, and a Sub with no
  candidate slot for a declared method) accepted `.wrap` but never consulted
  the wrapper. All of these now carry the same method-wrap registry slot that
  `.^method_table` entries already had.
- **`$method does R` lost the Method.** When R declares an attribute, the
  object is reblessed into the synthesized `Method+{R}` type. That type's
  declared attribute list holds only R's attributes, so `.name` and `.rw`
  (stored on the base Method) were rejected. The `.wrap`, `.unwrap` and
  `CALL-ME` handlers also matched only a class named exactly `Method`. A
  mixin type over a built-in base now reads the base's stored attributes, and
  the routine-object handlers accept mixin types derived from
  `Method`/`Submethod`/`Regex`.

Object::Permission goes from 4/6 to 6/6 baseline files at parity. The
regression test is `t/oo/method/wrap-reaches-accessor-and-rw-lvalue.t`. A
related gap turned up along the way: calling a `.^lookup` Method object
directly does not bind `self` for attribute reads. It is filed as #10083.
