# A `Nil` method called with arguments it refuses fails its signature

Rakudo reaches `Nil`'s `FALLBACK` only for a method name that no candidate
declares. mutsu absorbed any call that missed one of `Nil`'s rows, so
`Nil.Str(1)`, `Nil.Numeric(1)`, `Nil.uc(1, 2)` and `Nil.chars(1)` answered `Nil`
(#18575). Now a name `Nil` declares fails the call the way Rakudo's dispatcher
does:

- a bounded proto, or an only method, raises "Too many positionals passed";
- any other multi raises `X::Multi::NoMatch`, worded `Str(Nil:U: Int:D)` (it
  used to say `Nil:D`) and listing Rakudo's candidates.

`scripts/builtin-method-sigs.raku` now binds each type object instead of
assigning it. Assigning `Nil` stored `Any`, so `Nil`'s own candidates were
never listed.

Two methods also move to `Nil`'s rows:

- `Nil.chrs` warns about the numeric context, as in Rakudo (it said "string").
- `Nil.iterator` is the iterator of the one `Nil`; it used to be `Nil` itself.
