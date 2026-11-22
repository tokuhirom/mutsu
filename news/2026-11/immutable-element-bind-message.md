# Binding an element of a Map, List or Range raises rakudo's X::Bind

`$m<a> := 5` on a `Map` dies in rakudo with "Cannot bind to Map": the message
names the type through `Map.BIND-KEY`. mutsu named the variable instead
("Cannot bind to m", "Cannot bind to %h"). A `List` or `Range` element has no
`BIND-POS` slot at all, so rakudo's X::Bind there carries no target and reads
"Cannot use bind operator with this left-hand side". mutsu said "Cannot bind
to l" for a List. For a Range it raised X::Assignment::RO, as if the bind were
an assignment.
