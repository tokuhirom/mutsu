# Binding an element to its own `given` topic no longer overflows the stack

`%h<k> := $_ with %h<k>` (and the `given %h<k> { %h<k> := $_ }` /
`@a[i]` forms) binds the element to the topic that already aliases it, which
is a no-op in Raku. mutsu's element-topic writeback stored the topic's shared
container cell back through the element's own cell, so the cell contained
itself and the next read recursed until the stack overflowed (or, through a
reused hash, leaked the bound value into unrelated keys). The writeback now
stores the topic's value, never its container.

Found by the ecosystem roulette on PURL, whose canonicalization step does
`%args<name> := $_ with $customization.canonicalize-name(%args<name>)`; all
three of its test files now pass under mutsu.
