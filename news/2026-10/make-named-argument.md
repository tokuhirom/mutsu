# `make :name(value)` is a named argument, as in rakudo

`make` had its own one-expression argument parser, and the builtin accepted
any argument shape. Two problems followed. `make :s(1)` was quietly accepted:
the colonpair is a *named* argument, the `.made` it left was not even the
Pair, and rakudo rejects the call outright. And `make 1, :s(2)` parsed as
`(make 1), :s(2)`, which mutsu only reported as a sink warning.

`make` now parses its argument list like any other listop. The builtin
enforces rakudo's `sub make(Mu \made)` signature: a named argument (a plain
`Pair` at the call boundary, ADR-0021) dies with `Too few positionals passed;
expected 1 argument but got 0`, or with `Unexpected named argument 's'
passed` when there is also a positional. Any positional count other than one
is rejected too. `make (:s(1))`, `make "s" => 1` and `make $k => 1` still
make the Pair (#10523).

Pinned by `t/grammar/grammar-make-named-argument.t`.
