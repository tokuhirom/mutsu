# PDF `cos-type-xref`: exported code variables, lazy slices of an Array subclass, container Range endpoints

Three general gaps stood between `PDF::COS::Type::XRef.decode` and Rakudo.

A code variable a module exports (`our &unpack is export(:pack) = ...`) was
reachable by name, but a `unit class ... is Hash` file, whose `use` has a
lexical import set, read `$buf.&unpack(...)` through the core `unpack` (a Perl 5
pack template) instead. The import now binds the exported routine in the set.

`$a[1, 3 ... *]` on an `is Array` subclass instance died with "Cannot coerce an
infinite lazy list to a strict list"; it stops at the first index past the end,
as the Array slice does.

A Range endpoint that is the container an `is rw` `AT-POS` returned
(`callsame`) is the value it holds: `for 1 .. $w[1]` ran once instead of
twice.
