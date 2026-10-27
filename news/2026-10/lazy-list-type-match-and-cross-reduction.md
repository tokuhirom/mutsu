# Lazy lists, [X]/[Z] and itemized sequences read the way rakudo reads them

Four fixes found while running Red's cross-product tests:

- `$lazy-list ~~ Positional` (a type matcher) no longer reifies a gather that looks finite by shape,
  so `(lazy gather { loop { take 1 } }).List ~~ List` answers instead of spinning forever.
- `[X] a, b, c` and `[Z] a, b, c` hand their operands to the one n-ary `X`/`Z` call: a plain or lazy
  gather operand, an infinite range and a user `infix:<X>` behave as they do between two infixes.
- When a user `infix:<X>` declines its operands, the core product is one n-ary product over the operands
  as written, so `(A X B) X C` keeps its nested tuples instead of being flattened by a pairwise fold, and
  an infinite operand stays lazy.
- A method call on an itemized `Sequence` instance (`$resultseq.item.list`) reads the sequence, not a
  one-element list holding it.
