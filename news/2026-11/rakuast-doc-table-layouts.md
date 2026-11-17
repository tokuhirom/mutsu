# RakuAST: more Pod table layouts are read, and a table deparses

The RakuAST view used to leave several Pod `table` blocks out of a unit's
statements (#14880). Those blocks shifted every later statement of the file in
the `.AST` and `.DEPARSE` corpora. They are now read the way rakudo's RakuAST
frontend 2026.09 reads them; the measurements come from rakudo:

- **Indented tables.** A table without blank lines drops the indentation its
  lines share, and a pipe row indented further keeps the rest. A table with
  blank lines keeps its indentation in its first cells.
- **Columns.** A whitespace table's columns start after a run of two or more
  blanks in any row, not the run a line is indented by. When two starts are one
  column apart, the later one wins. Every row is cut at the same offsets, so
  rows whose columns differ are read too.
- **Runs of rows.** Between several separators each run of rows is one
  multi-line row. A first separator unlike the second makes the first run a
  header.
- **Pipe tables.**
  - `+` divides a row of a pipe table.
  - A rule may cross the dividers with `|` (`==|==`).
  - A pipe header can span lines.
  - Cells may be blank.
  - A divider that ends the line divides a row of one cell.
- **Configuration.** A block's configuration continues on `=` lines.

`RakuAST::Doc::LegacyRow.Str(&divider)` puts a row back as written and passes
each divider through the callable, which the vendored deparser uses to
highlight it. A table block now deparses.

`roast/S26-documentation/07-tables.t` now matches rakudo statement for statement
in both corpora: 53 statements were added to each `identical.tsv`.
