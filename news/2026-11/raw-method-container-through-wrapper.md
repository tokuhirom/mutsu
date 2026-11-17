# An `is raw` method keeps its container through wrappers, `nextcallee` and `++`

Method::Protected's `t/01-basic.rakutest` died at
`$grabonizer.it("zzz")++` with `Cannot modify an immutable Int`. The method is
`method it(str $key) is raw is protected { %!hash{$key} }`. The `is protected`
trait wraps it with `my method (|c) is raw { my &original = nextcallee;
self.LOCK.protect: { original(self, |c) } }`, and it picks that raw wrapper
only when `$method.rw` is true.

Five gaps lost the container on that path:

- **`.rw` ignored `is raw`.** `.rw` answered `False` for an `is raw` routine,
  whether a `Sub`, a method from `.^lookup`, or the code object a
  `trait_mod:<is>` handler receives. rakudo sets the same `rw` flag for
  `is raw` as for `is rw`, so mutsu now does too.
- **The `nextcallee` continuation was not raw.** For a method, `nextcallee`
  returns a continuation, and its template `sub (|c) { ... }` decontainerized
  its result. It is now `is raw`, and the candidate callable keeps the
  method's own `is raw`.
- **`++` through an `&`-variable accepted only a `Sub`.** `o($obj, ...)++`,
  where `&o` holds a `Method` object, fell through to a name lookup
  ("Unknown function"). It now steps through the method's container when
  the method is rw.
- **`++` on any other call always raised.** `$obj.&m(...)++` compiled straight
  to the "requires mutable arguments" error. Raku's `++` takes its operand
  `is rw`, so the operand now decides: a returned container (a `Scalar` cell
  or a `Proxy`) is stepped, and anything else still raises the same
  `X::Multi::NoMatch`.

Method::Protected `t/01-basic.rakutest` now passes 4/4, as on raku.
