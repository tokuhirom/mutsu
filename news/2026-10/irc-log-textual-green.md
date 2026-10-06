# IRC::Log::Textual passes under mutsu

Taking the `IRC::Log::Textual` distribution from `red` to passing both of its test files
(73 and 46 assertions) exposed four interpreter gaps, each now fixed generally and pinned in `t/`:

- **Multi-method tie-break on optional positionals.** `multi method new(C:U: Str:D $a, Int() $b = 5)`
  next to an inherited `new(::?CLASS:U: Str:D $a)` raised `X::Multi::Ambiguous`. As for subs, a
  candidate whose positionals are all required is now narrower than one that can omit some.
- **`IterationBuffer` answers `Any`'s list methods.** `head`, `tail`, `first`, `skip`, `grep`, `map`,
  `sort`, `min` and `max` run over its elements instead of treating the buffer as one item.
- **A bound Seq is no longer sunk by a native-typed pointy `if`.** `if $c -> str $t { $seq := $seq.reverse }`
  lowers to an anonymous-block call; the statement sank the Seq it had just bound, so a later use
  died with "iterator already in use".
- **Subscripting by an `Int` mixin or `is Int` subclass instance.** `@a[1 but R]` and
  `@a[nqp::box_i(1, NotFound)]` (what `Array::Sorted::Util.finds` returns) read the slot instead of `Nil`.
