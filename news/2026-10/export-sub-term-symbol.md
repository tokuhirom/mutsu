# Terms exported through `sub EXPORT`

A `sub EXPORT` map entry `'&term:<today>' => &today` now makes the bareword
`today` a term in the importer, the same way binding `my &term:<today>` or
importing an `is export` `sub term:<today>` does:

- at run time the import also binds the bare symbol, so `today` calls the
  routine instead of dying with "Unknown function: today";
- at parse time the export-hook scan collects operator and term names spelled
  as literal pair keys of the returned `Map`, so `today + 1` parses as a term
  plus one rather than the listop call `today(+1)`.

The Today distribution exports its term this way; its test now passes.
