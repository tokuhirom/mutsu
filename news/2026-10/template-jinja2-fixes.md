# `try { return $failure }` throws into the try; accessor signatures count 1

Two gaps found by the ecosystem roulette on Template::Jinja2:

- A `try` block is a `use fatal` scope, so a Failure produced inside it is
  thrown there. `try { return $s.Num }` with a non-numeric `$s` now lands in
  the try (which yields Nil) and the routine carries on, instead of returning
  the Failure — the `float` filter's fallback idiom.
- An auto-generated attribute accessor's Method object now has Rakudo's
  `(Class:D $:: *%_)` signature, so its `.count`/`.arity` are 1 rather than
  Inf; Jinja2 uses `.can($attr)[0].count <= 1` to decide whether a host
  object's member is an attribute.

`t/11-ported-filters` and `t/24-python-methods` now pass under mutsu (23 of
24 files); `t/12-advanced-tags` needs #11304.
