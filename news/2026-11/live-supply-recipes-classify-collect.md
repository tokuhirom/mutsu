# Live `classify`, `categorize` and the collecting combinators are per-tap recipes

ADR-18443 slice 3b. Over a live source (or a recipe built on one):

- `Supply.classify` / `.categorize` give each tap its own groups, each still
  backed by a preserving supplier, and the groups are done or quit with the
  source, before the classified supply itself.
- The combinators that emit only once their source is done -- `sort`,
  `reverse`, `rotate`, `collate`, `grab`, `comb`, `split` -- collect per tap
  and run over what that tap saw. Over a plain `Supplier`'s supply they used
  to snapshot the (empty) values when the method ran, so
  `$supplier.Supply.sort` emitted nothing.

Neither runs anything until it is tapped, and closing the tap releases the
source.
