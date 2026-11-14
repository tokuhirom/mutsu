# A parenthesized expression in sink context is not reported

rakudo reports no "Useless use of ... in sink context" for a parenthesized
statement: `(5);`, `(1, 2);` and `(1 + 2);` are silent. Only an empty `()`
still warns. mutsu looked through the parentheses and warned for every
element, so a statement like `(@x.map({ ... }), 5);` printed a warning
rakudo does not.

The sink-warning analysis now stops at the parentheses. A comma list
without them still distributes the sink: `(1, 2), 3;` reports the `3`
alone, as on rakudo. Test: `t/control/sink-warning-parenthesized.t`.
