# RakuAST keeps the shape of an assignment's left side

`.AST` now records the left side of an assignment the way rakudo 2026.09
does (#14911). A prefix (`-$x = 5`), ternary (`$y ?? 1 !! $x = 2`) or named
capture (`$<c> = $0`) target is the node itself instead of a grouped call,
and a parenthesised variable (`($x) = 1`, `(@a) = 1, 2`, `(%h) = a => 1`)
stays a `RakuAST::Circumfix::Parentheses`, which makes it a list assignment
(`Assignment.new`, not `Assignment.new(:item)`). These trees lower back to
the same code, and rebuilding them from their `.raku` gives the same tree.
