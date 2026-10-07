# Terminal::UI's own test suite passes

Taking `Terminal::UI` from `partial` (7/10 baseline files) to all ten files passing exposed four
general interpreter bugs:

- An EXPORT-hook term (`Terminal::ANSI::OO`'s `t`) is now also installed in, and read from, the term
  namespace, so a caller's `my $t` can no longer answer the bareword `t` inside an attribute default.
- A routine run as a TRIR chunk now honours the compunit-cell redirect (#11275) for accessor-assign
  writebacks, so `$state.n = 1` in one module no longer replaces a same-named closure variable of
  another (Terminal::ANSI's `set-scroll-region` clobbering Terminal::ANSIParser's parser state).
- `.kv` / `.values` / `.pairs` on an Array reached without a variable name (`@$a.kv`, `$a.list.kv`)
  hand out the element containers, so `for @$a.kv -> $i, $v is rw { ... }` writes the array.
- `Array.clone` gives the clone containers of its own instead of sharing promoted or `:=`-bound cells.
