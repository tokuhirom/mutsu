# `for $_` under `given @array` iterates the elements

`given @d { for $_ -> $a { ... } }` iterated the array once as a single item because the compiler
always wrapped a bare `$_` loop source in a one-element list. `given @d` binds the topic to the
Array itself (no Scalar), so rakudo iterates its elements; only an itemized topic (`given $x`,
`for $x { for $_ {...} }`) is one item. The wrap is no longer applied to `$_`, so the value's own
itemization decides. Found via `Data::Transformers` (`accumulate` on a matrix).
