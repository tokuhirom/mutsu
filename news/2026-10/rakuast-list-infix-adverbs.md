# Adverbs on a list-associative infix cross the RakuAST boundary

`$z & $y :adverb` passes `:adverb` to a user-declared `infix:<&>`. Because
`&` is list-associative, the application is a `RakuAST::ApplyListInfix`, and
the converter refused one that carried adverbs. That kept
`roast/integration/advent2013-day10.t` from round-tripping under
`MUTSU_RAKUAST=1`.

Rakudo 2026.09 stores these adverbs in the node's private `$!adverbs`. They
are not in its constructor form, have no accessor and are skipped by
`visit-children`, but `EVAL` of the node still passes them on. mutsu now
keeps them the same way, in a hidden `adverbs` field, so `.AST` prints
exactly what rakudo prints and the round trip still calls the operator with
the adverb.

The nodes also gained rakudo's mutator: `ApplyInfix.add-colonpair($pair)`
adds a rendered colonpair, and `ApplyListInfix.add-colonpair($pair)` adds a
hidden adverb.

One related wrong answer is left as #14630. With a trailing adverb, mutsu
calls an overloaded `&` pairwise over three operands; rakudo makes one call
with all of them.
