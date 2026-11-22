# `Signature eqv` compares structure, not text

`:(Int $x) eqv :(Int $y)` is `True` in Rakudo, because `Signature eqv` is
structural. Two signatures match when they have:

- the same return type and number of parameters;
- positionals that match pairwise by nominal type, flags (sigil, optional,
  slurpy kind, traits), sub-signature and literal constraint;
- nameds that match by name, in any order.

A positional parameter's name does not count, and a `where` block never
matches. mutsu compared the two signatures' `.raku` text, so every renamed
positional made them differ (#18679), for example
`&f.signature eqv &g.signature` for `sub f(Int $a)` and `sub g(Int $b)`.

The stricter comparison exposed that a signature's `.raku` left out a
non-literal default (`:(@a = [2]).raku` was `:(@a)`), so `.raku.EVAL` rebuilt a
required parameter. It now renders as Rakudo's does: `= Code.new`, with a type
object default shown by name (`= Int`).
