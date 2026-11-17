# RakuAST: `:5($n)` is a `Term::RadixNumber`

A radix conversion written with parentheses, `:5($n)`, viewed in RakuAST as
the call the parser runs it as: `UNBASE(5, $n)`. `.DEPARSE` printed that
internal call. Rakudo's RakuAST frontend keeps a `Term::RadixNumber` with the
radix, `multi-part => False` and the parenthesized value.

mutsu now has the `RakuAST::Term::RadixNumber` class: its named
constructor, the `radix` / `multi-part` / `value` accessors (regenerated
with `scripts/gen-rakuast-accessor-rows.py`), and `.DEPARSE` through the
vendored deparser. The parser marks the conversion with
`Spelling::RadixNumber`, the view renders the term
(`src/rakuast/radix_number.rs`), and lowering maps it back to the marked
call. Found through the `.DEPARSE` corpus
(`integration/advent2013-day15.t`, #14880).
