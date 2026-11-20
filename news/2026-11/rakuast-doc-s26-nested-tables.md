# RakuAST Pod: rakudo's table algorithm, nested indentation, `# ` numbering

A differential over every `roast/S26-documentation/*.t` file (the `.raku` of
each `Doc::Block` statement, rakudo 2026.11 against mutsu) went from 20 to 24
identical files of 27 (#14880):

- **Tables** are now read with a port of rakudo's own
  `RakuAST::Doc::Block!interpret-as-table` (`Fixups.rakumod`): the common
  leading whitespace is removed, the lines are read assuming whitespace
  column dividers (a column offset counts only when no row has text two
  positions before it), a visible `|` / `+` divider switches to reading by
  dividers, rows between dividers are merged into multi-line rows, and the
  legacy heuristic decides `header-row`. mutsu's reader matched measured
  shapes only and gave up on a row with `-`, `=` or `_` in it
  (`-r0c0  r0c1`); a cell holding a formatting code is still left out.
- A line indented beyond its block's margin keeps its extra blanks in a
  `head`, `para` or custom block and loses them in `item` / `pod` / `nested`
  (`=begin foo` … `    =bold Turn` / `          into`), for abbreviated,
  `=for` and indented blocks alike.
- `=item # one` / `=defn # term` are numbered (`:numbered(1)`).
- A balanced `<...>` inside a formatting code is its text, codes in it still
  read (`C< infix:<+> >`, `C<< < B<foo> > >>`).
- `E<LATIN CAPITAL LETTER A>` resolves a Unicode character name.
- An indented `=config` directive is read.
