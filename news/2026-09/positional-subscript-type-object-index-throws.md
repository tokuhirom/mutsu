# A positional subscript indexed by a type object throws instead of answering Nil

`@a[$i]` (also `:exists` and `:delete`) with an undefined `$i` — an
uninitialized `my $i;`, or a bare type used directly (`@a[Int]`) — used to
fall through mutsu's positional-index dispatch to a catch-all default and
silently answer `Nil` (or `False` for `:exists`), instead of raku's
`postcircumfix:<[ ]>` refusing to index with a type object at all
("Unable to call postcircumfix ... with a type object" / "Indexing requires
a defined object").

A Failure held in the index (`@a[1 div 0]`) was silently dropped too,
rather than propagating the exception it wraps, so `try @a[1 div 0]` never
set `$!`.

Separately, a Callable/WhateverCode index (`@e[{0}]`, `@a[* div 2]`) that
lands out of range read back as the internal `Nil` sentinel instead of the
array's own `Any` default — the same value an out-of-range plain `Int`
index already answers.

All three are now fixed in the VM's positional-index read, `:exists` and
`:delete` paths, sharing one `is_type_object_index` check.
