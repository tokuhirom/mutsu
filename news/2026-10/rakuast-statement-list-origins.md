# RakuAST statement and statement-list ranges skip comments

`.AST` placed a statement and its statement list differently from Rakudo
whenever comments or spaced terminators were involved:

- a statement followed by a comment (`1 # note`) took the comment into its
  range, because the statement parser returns the input after the
  whitespace and comments that follow it;
- a block's or unit's `StatementList` stopped before a `;` written after a
  space (`1 ;`), and the `SemiList` of `( )` and `[ ]` started after the
  blank text of an empty list instead of just after its bracket;
- the `StatementList` of an expression block holding a comment
  (`{ # c ... }`) had no range at all.

Now `ws` records each run of whitespace in which it consumed a comment or
Pod, and a statement's end steps back over such a run only when one ends
exactly there, so a `#` inside a string is never taken for one. A list runs
from its first statement to the `;` after its last, as Rakudo's does, and is
left without a range when anything but whitespace and plain comments
surrounds its statements (Pod and declarator comments, which Rakudo keeps as
nodes of their own). The same reading of comments lets a comma list with
comments between its operands keep its range.
