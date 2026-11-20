# A routine with a `for` loop keeps the fast call paths

`detect_inner_subs` counted every `ForLoop` op in a routine body as an inner
sub, on the theory that the loop's FIRST/NEXT/LAST phasers needed state only
the full call path sets up. That flag shuts a routine out of the
zero-argument fast path, the light named and positional paths, the scoped
fast frame and TRIR, so any sub with a `for` loop in it paid the general
call on every call. The phasers are compiled into the loop's own body and its
iteration state lives in the `ForLoop` op; nothing reads the routine-registry
snapshot or the routine frame the full path adds. The arm is gone.

A `for` loop also read its tagged source's binding by name four times at
entry (the immutable-QuantHash, immutable-Map and mutable-QuantHash checks,
and the topic's deep-readonly check, which is the same test as the first);
it now reads it once.

Per call, with a one-element loop in the body (callgrind slopes, profiling
build): `sub f(@p) { for @p { } }` 58.4k to 16.5k instructions,
`sub f() { my @a = 1; for @a { } }` 39.9k to 27.9k, a sub looping over an
outer array 34.3k to 19.4k, and `for ^1 { }` 17.6k to 8.7k. A method looping
over an attribute went from 29.3k to 24.2k. MCP::Server::Tool::Web's
html-to-text moved by 0.2%; its hot helpers loop with `while`.
