# Declarator docs are attached by the parser

`#|` and `#=` declarator comments used to be attached by `collect_doc_comments`, a ~1340-line
scanner that re-read the program source line by line, guessed declarations from the text, and
let `.WHY` find an anonymous sub's doc by counting `&<anon>` lines or by source-line proximity.
Every new comment layout needed another heuristic — the latest being a rewrite pass for a `#|`
written after `=` (roast `dd85d3c9`).

The parser now decides it (ADR-0134, #10226). `ws` records each declarator comment it skips,
every declaration records its extent when it parses, and once the unit is parsed a `#|` goes to
the next declaration to start after it and a `#=` to the latest declaration still claiming it.
Positions are source offsets, so backtracking and memoization cannot disturb the result.
Anonymous subs and blocks carry their documentation on the AST node and then on the closure's
compiled code, so `.WHY` reads it from the code object.

Visible changes:

- `#| doc` above `my $x = anon sub {}` documents the variable, not the sub, as in Rakudo main;
  `my $x = #| doc\n anon sub {}` documents the sub.
- A `#|` documents the next declaration wherever it starts, e.g. an anonymous sub passed as an
  argument (`#| doc\nis sub { }.WHY, ...`).
- `#=====` separator lines and `#|x` are plain comments, as in Rakudo.
- An `@`/`%` attribute's doc is found under its own sigil.
