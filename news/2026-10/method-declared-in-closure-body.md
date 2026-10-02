# A `method` declared inside a closure in a class is installed

A `method` declared inside a closure in a class body is now installed in the class, as in
Rakudo (#10820). This covers a pointy block, an anonymous `sub` or `method`, a bare `{ }`
term, `do`, `try`, `gather` and `once`. The method closes over the closure's latest run:
`method mk { for 1, 2 -> $k { -> { method km { "km $k" } }() } }` leaves `km` answering
`km 2`.

The parser's nested-method hoisting already followed nested statement blocks and routine
bodies. It now also scans the closures a statement's expressions build. The `do { }`
statement form moved from the block list to that closure scan.
