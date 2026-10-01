# Quantified backreferences match

A quantifier after a backreference (`$0*`, `$0+`, `$0?`, `$0 ** 0..3`, `$<name>*`) never took
effect: the regex parser pushed the backreference and moved straight on, so the quantifier was not
read and every such pattern failed to match. `"aab" ~~ /(a) $0+ b/` was `Nil`; rakudo matches
`aab`. A backreference now goes through the same atom path as every other atom, which reads its
quantifier, its frugal marker and its separator.

Those patterns were also the main reason the compiled regex engine (ADR-0135) still declined
`nullable-loop` bodies. A loop whose body can match empty and which the walk grows one first
candidate per iteration now compiles, each iteration committed the way the walk commits it.

Part of [#10255](https://github.com/tokuhirom/mutsu/issues/10255).
