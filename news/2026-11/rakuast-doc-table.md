# Pod `table` blocks read as `RakuAST::Doc::LegacyRow` rows

A `=begin table`, `=for table` or `=table` block between the statements of a unit
is now a `RakuAST::Doc::Block` of `RakuAST::Doc::LegacyRow` rows in its `.AST`
(#14880), with the new node class (`column-dividers`, `column-offsets`, `cells`;
`column-offsets` prints inline as `(0,3)` / `(4,)`, an `array[int]`).

Rakudo infers a table's columns with a heuristic whose source is not available,
so only the shapes measured and checked against `raku` over about 1,000 generated
tables (and 4,800 random ones, to catch wrong trees) are read; everything else
leaves the block out of the tree, as before:

- whitespace tables: cells start after a run of two or more spaces, every row at
  the same offsets, a cell running to two characters before the next offset;
- pipe tables: a `|` between blanks is a divider, each row has its own offsets;
- blank lines and rules (`--`, `==`, `-- --`, `--+--`) between rows are strings,
  one interior separator joins the rows before it into a header row, and
  `header-row => 0` is added to the block's config unless two equal rules rule
  every row off alike.

`t/rakuast/rakuast-doc-table.t` runs identically under `raku`.
