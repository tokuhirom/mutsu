# A qualified `Mu::` call answers as Rakudo's `Mu` does

`$obj.Mu::gist`, `$obj.Any::Str` and the other qualified calls to `Mu`'s
methods are now answered by `Mu`'s base rows, which are also the end of a
`callsame` chain (ADR-11276 §9.150).

Two answers change to match Rakudo:

- `$obj.Mu::gist` is the object's own `raku`. It used to be the generic
  `Class.new(...)` rendering.
- `$obj.Mu::Str` is `Class<id>`. It used to be `Mu<id>`.

The qualified form also reaches `so`, `not`, `WHERE` and the output methods
now. The two hand-written `match`es that answered these calls are gone.
