# RakuAST: a seventh set of origins from the roast corpus, and two parser fixes

The `.origin` differential (#15780) over the roast sample found more unplaced
nodes, now placed as Rakudo places them: a quoted `handles 'uc'` on a method,
the return type of `anon Str sub { }`, the statement list of a block of empty
statements (`{ ;  ; }`), a parameter's `where { ... }` block, and a parameter
default whose operands are all `*` (`$x = * <=> *`).

Two of its differences were wrong answers in the parser:

- **A statement prefix takes the whole comma list.** `say try 1, 2` said `12`
  and `my $x = quietly 1, 2` sank the `2`: `try`, `quietly` and `once` in an
  argument or initializer position took one expression. They now take the
  statement's comma list, so these say `(1 2)`, and `(1, once 2 ... 5)` keeps
  the sequence inside the `once`.
- **A hash composer keeps its elements in order.** `{ a => 1, $k, 2 }` failed
  to parse with "Malformed initializer". After the first bare element, every
  element now stays where it was written, and the composer pairs them in that
  order. `{ a => 1, "b", 2 }` also keeps `"b"` as an element of the list, which
  is the shape Rakudo's AST has.
