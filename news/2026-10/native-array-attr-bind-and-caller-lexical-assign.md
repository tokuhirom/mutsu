# Native array attribute binds and `CALLER::LEXICAL::<$x> = v`

Found by the DirHandle / P5opendir ecosystem suite.

- Binding a native array (`my str @x`) into a same-typed attribute (`has str @.items; @!items := @x`)
  no longer drops its `array[str]` declared type, so `nqp::atpos_s` and friends accept it.
- `CALLER::LEXICAL::<$name> = value` now assigns the caller's lexical (any lexical, not only a
  dynamic one) from a sub or a method.
