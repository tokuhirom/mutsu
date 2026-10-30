# PDF::COS: hash-subclass entries stay containers, and a role mixed into a hash keeps `callsame`

Three gaps kept PDF 0.6.17's `PDF::COS::Tie::Hash` from matching rakudo; its own `t/cos-coerce.t`,
`cos-deref.t`, `cos-text-string.t` and `cos-type-info.t` now match `raku`:

- `callsame` from a role's `AT-KEY` on an `is Hash` instance with another role mixed in (`$dict but
  Info`, what `.^mixin` produces) answered Nil: the dispatch frame's native storage candidate looked
  only at a bare instance. It now sees through the mixin.
- `self.BIND-KEY($key, $lval)` inside an `is Hash` class bound the variable's value, so the element was
  not a Scalar. A variable source now binds as a writable Scalar, as `callsame` out of `AT-KEY` expects.
- An object of a class that inherits a builtin (`class TextString { also is Str }`) failed to bind to
  `sub f(Str $s --> Str)`: the light call path refused any object for a builtin nominal type. Its MRO
  now decides.
