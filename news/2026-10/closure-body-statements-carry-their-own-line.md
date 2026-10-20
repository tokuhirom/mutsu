# A closure body's statements carry their own source line

A `{ ... }` block or `-> { ... }` pointy block used as an expression inherited the line of the statement that contains it. A call made from its body therefore recorded that line, and `callframe($n).line` of the closure's frame (BDD::Behave's `expect` walks to it to report where an expectation failed) was the line that started the enclosing statement, not the line being executed. Such a body now keeps a line marker per statement, as a statement block does. `ENTER` expressions hoisted out of a block ignore the markers.
