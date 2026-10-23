# `for @array` no longer promotes every element when the body cannot see the alias

`for @a { ... }` binds `$_` to the element's own container, which mutsu does by
promoting each element to a `ContainerRef` cell: an allocation, a GC object and
a drop per element, plus a by-name re-resolution of `@a` per iteration. On
`for @src { $n += $_ }` over 300k Ints that was ~4x rakudo (#14495).

`compiler/for_topic_alias.rs` now proves, per loop, that the body cannot tell
the container from a copy of its value: an allow-list of statements and
expressions (scalar reads, the arithmetic/comparison/string operators, plain
and compound assignment to a scalar other than `$_`, `say`/`put`/`print`/`note`,
`if`, `next`/`last`) that cannot write, bind or capture `$_`, run user code or
reach the source array. Such a loop (`ForLoopSpec::topic_alias_unobservable`)
binds the plain value, whose writeback is a no-op for an unchanged topic.
Every other body keeps the alias. Release, 300k elements, second run: 0.5-0.58 s
before, 0.11-0.15 s after, against rakudo's 0.12-0.17 s on the same box.
