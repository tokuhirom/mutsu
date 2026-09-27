# Lazy slipped list tails, and the value of `my $x = 1, 2`

Math::Handy's suite now passes in full (17/17; before, it died after 40 seconds with
`memory allocation failed`). Two general interpreter gaps caused this.

**A list literal with a slipped lazy list is now lazy.** `(1, |[\*] 1..*)`,
`(1, |(1...*))`, `(1, |(1..*).map(...))` and `(1, |(lazy gather ...))` are
lazy lists in Rakudo, and they reify only what is read. mutsu's `|` used to force a
200,000-element prefix of an infinite scan. For a factorial table that runs out of
memory. A slipped map pipe or gather was also kept as one opaque element, so
`(1, |(1..*).map(* + 0))[3]` read `Nil`. Now `|` keeps a genuinely lazy list intact.
The list constructor then builds a lazy concatenation from the result: a new `Concat`
pull adaptor that reads each run of plain elements and each slipped lazy part in order.
`.is-lazy`, indexing, `.head`, `Z`, and assignment to `@`-arrays all follow Rakudo.

**`my $d = 6, 3` has the whole list as its value.** Item assignment binds tighter
than `,`, so a routine that ends in `my $div = ($n / $m).Int, $n % $m` returns
`(6 3)`. mutsu returned just `3`, because the trailing items were separate sink
statements. They now form one list headed by the declared scalar. The sink-context
warnings are unchanged: they still name only the trailing items.
