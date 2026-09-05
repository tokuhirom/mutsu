# RakuAST renders `until` as `until`

`until X { }` and `repeat { } until X` were *wrongly rendered* rather than
refused: mutsu stores an `until` as `while !X`, and `.AST` rendered exactly
that — a `Statement::Loop::While` over an `ApplyPrefix("!")`. raku has
`Statement::Loop::Until` and `Statement::Loop::RepeatUntil` classes and renders
the **undecorated** condition.

The information was there all along. `Stmt::While` and `Stmt::Loop` both keep an
`is_until` flag alongside the negated condition — the converter simply did not
read it, and its comment ("mutsu desugars `until X` to `while !X`") recorded the
desugaring as if it were lossy. The converter now picks the class from the flag
and strips the `!` the parser added; the lowerer re-plants it. A flagged loop
whose condition is *not* a negation is refused rather than rendered, since that
would mean the flag and the negation had drifted apart.

Measured against rakudo 2026.07: the gists are byte-for-byte identical for
`until`, `repeat ... until`, and both `while` forms.

## How it was found

A gist-comparison sweep: render a corpus of small programs under both mutsu and
rakudo and diff, reporting only cases where **both** render — a mutsu boundary
is honest, a mutsu *disagreement* is a bug. `until` was one of four hits.

The other three need the parser to stop erasing a distinction, and are filed with
their measured rakudo shapes:

- `todo/tickets/rakuast-fat-arrow-key-spelling-swapped.md` — `a => 1` and
  `"a" => 1` render as each other's node. `PositionalPair`, which the converter
  keys on, means "parenthesized pair", not "quoted key".
- `todo/tickets/rakuast-unless-and-parens.md` — `unless` renders as a negated
  `if` (`Stmt::If` has no `is_unless` flag, unlike `Stmt::While::is_until`), and
  `(1, 2)` loses its `Circumfix::Parentheses` wrapper.

## Coverage

`t/rakuast-until-loop.t` (11 assertions) pins both `until` classes, the
undecorated condition, that neither renders as a `While`, that both `while` forms
are unchanged, and four `EVAL` round trips including an `until` whose condition
is already true. It is a dual-oracle test: it passes verbatim under both mutsu
and rakudo 2026.07.
