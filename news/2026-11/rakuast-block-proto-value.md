# A block-local multi's dispatcher has its value at CHECK time

Since #17836 a call of a `multi` resolves (in `$*CU`) to its dispatcher, the
scope's `proto` or the one its first candidate implies. For a `multi` declared
in a nested block that dispatcher is not installed until the block runs, so
its `compile-time-value` was `Nil` at CHECK time; rakudo 2026.09 answers the
dispatcher over the block's candidates.

The resolver now gives each such dispatcher stub the scope's candidates of its
name (body-less, in declaration order), and `compile-time-value` builds the
dispatcher from them while the block has not run: `is_dispatcher` is True and
`.candidates` are the block's candidates, each built from its declaration.
Part of #17826 (a block candidate added to an outer family, which rakudo
answers as a merged `Implicit::Constant`, is still the candidate itself); the
regression test is `t/rakuast/rakuast-block-proto-value.t`.
