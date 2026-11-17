# `$block ~~ 2` dies as in rakudo: a numeric-literal smartmatch numifies its topic

Rakudo compiles a smartmatch whose right side is written as an `Int` or `Num`
literal (`~~ 2`, `~~ -2`, `~~ 2e0`, `when 2`) as a numeric comparison against
the topic's `.Numeric(:fail-or-nil)`. So a topic whose type declares no defined
`Numeric` — a `Block` or `Sub`, a `Pair`, a bare `Mu` or `Exception`, an object
of a plain user class (even an `is Cool` one) — dies with
`Cannot resolve caller Numeric(Block:D: :fail-or-nil)`, and a `Failure` topic
throws its exception; the same match against a variable (`$b ~~ $two`) goes
through `Int.ACCEPTS` and is just `False`. mutsu answered `False` everywhere,
which is how `given $number { when 2 { ... } }` with a `{...}` topic in the
Haskell-to-Raku guide printed "other" instead of dying.

The compiler now marks a `SmartMatchExpr` whose RHS is such a literal
(`rhs_numeric_literal`), and the `~~` opcode and a `when` with a literal
matcher run one check before matching. `Inf`/`NaN` (terms) and `Rat`
literals keep the ordinary match, as in rakudo. Found on the way: `set(1,2) ~~
2` and `(1,2).Seq ~~ 2` should numify to their element count (#17915).
