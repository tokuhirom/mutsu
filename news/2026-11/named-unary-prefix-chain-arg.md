# `lc ~?$b` takes `~?$b` as its argument

DataStar's `t/1-patch-elements.rakutest` died with `Unknown function: lc`. Its
helper is `sub js-bool(Bool $raku-bool) { lc ~?$raku-bool }`, and the parser
read that as the bareword `lc` concatenated with `?$raku-bool`.

A named unary builtin (`lc`, `uc`, `abs`, ...) is a listop. It already took a
single glued prefix as its argument (`uc ~$x`, `abs -3`). Two cases failed:

- a *chain* of glued prefix operators (`lc ~?$b`);
- a prefix glued to a bareword term (`lc ?True`, `lc ~?True`).

Both now open the argument, as in raku. The chain must start with `~`, `-`, `+`
or `?`. `!` is left out because `!eq` is a negated infix. `~~` and `??` are
excluded because they are infixes. A spaced prefix (`abs - $x`) is unaffected.

Both DataStar test files now match raku.
