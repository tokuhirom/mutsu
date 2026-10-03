# A bare type in a grouped declaration is an anonymous placeholder

`my ($q1, Any, $q2) = quartiles($x)` used to die at parse time with
"Confused. Two terms in a row": the grouped-declaration parser only accepted a
type when a variable followed it. A bare type in that list is an anonymous
typed scalar, the same as `Any $`. It takes one element from the right-hand
side and discards it after type-checking it, under both `=` and `:=`.

This was the only thing that stopped the `Stats` module from loading, so
`Data::Summarizers` failed to load as well. All six of its test files now pass
under mutsu.

The destructuring parser module was also split: element traits and nested
groups moved to `elements.rs`, and named destructuring moved to `named.rs`, so
that `mod.rs` stays under the 500-line limit.
