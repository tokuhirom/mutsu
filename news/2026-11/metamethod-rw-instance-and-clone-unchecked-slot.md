# `$obj.^meta = v` on an instance, and `.clone` over an unchecked MOP slot

Two gaps hit by RedFactory 0.0.3 (#16835):

- `$obj.^model = P` with a user `method ^model($obj) is rw { $ }` died with
  "Too few positionals passed" when the invocant was an instance; only a type
  object reached the metamethod. The instance now travels as the first argument,
  as in the rvalue call.
- `.clone(attr => v)` type-checked the twiddle against the declared attribute
  type even when the slot held a value that never satisfied it. rakudo checks
  against the container already in the slot, and `Attribute.set_value` (used by
  MOP-built attributes) binds without a check, so a `Str` attribute can hold a
  Block and be cloned with another one. The twiddle check is skipped when the
  slot's current value does not conform to the declared type.

The remaining RedFactory failure (a counter written by a method of a class nested
in a `unit class` module is reset after load) is #16843.
