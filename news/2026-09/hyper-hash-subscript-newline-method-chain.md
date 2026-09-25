# A hyper hash subscript's `}` no longer ends the statement at a newline

#9330 taught the parser that the `}` closing a hash subscript (`%h{"a"}`) is
not a block boundary, so Raku's line-ending-block rule must not end the
statement there and a `.method` on the next line keeps chaining. The hyper
form was left out: `@rows>>.{0..4}` (or `@rows»{1}`) is parsed as a hyper
`AT-KEY` call, not an `Index` node, so the shared `is_subscript_expr` helper
did not recognise it.

`taurus` (`lib/Taurus/CLI.rakumod`) writes exactly that:

```raku
@logs = @($log.IO.lines.skip.hyper.map({$p.parse($_)})>>.{0..4}
          .grep(*.[1].chars >= $digits));
```

and failed to load with this #7988 cluster's generic `Confused. expected
statement: ...` message. Outside parentheses the same shape ran and gave a
wrong answer: `@rows>>.{0..1}` newline `.elems` evaluated `.elems` as a new
statement on `$_`.

`is_subscript_expr` now also accepts a `HyperMethodCall` named `AT-KEY`, which
is what `>>.{...}` / `»{...}` produce; a user-written `>>.AT-KEY(...)` ends in
`)`, never in `}`, so the brace rule never consults it. The #9330 regression
test (`t/collections/subscript/hash-subscript-brace-newline-continues.t`)
gained three hyper cases. `taurus` now loads; its `t/01-seconds-to-str.rakutest`
still differs from rakudo on the wording of a `UInt` binding failure, a
separate gap.
