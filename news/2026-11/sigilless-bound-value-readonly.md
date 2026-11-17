# A sigilless term bound to a plain value refuses `+=` and `is rw`

A sigilless term bound at run time to a plain value is readonly, as on
rakudo. `my \h = $map<a>` is such a term: a Map element is a value, not a
container. Two ways of writing to it used to succeed and change the term:

- `h += 1`, and any other assignment written as an expression, now dies
  with `X::Assignment::RO`. The compiler emits the same `CheckReadOnly` the
  statement-form assignment already did.
- Passing `h` to an `is rw` parameter now dies with `X::Parameter::RW`. The
  term's `WrapVarRef` hands over its value, and the binder reads the term's
  readonly marker.

A term bound to a writable container (a Hash element, or a `$` variable)
still writes through.
