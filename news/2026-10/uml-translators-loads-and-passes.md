# UML::Translators loads and its whole suite passes

The `UML::Translators` distribution did not even load: its namespace walker
writes `my $pkg2 = $pkg::.WHO;`, and mutsu read the trailing `::` as a second
term. Rakudo treats a variable name ending in `::` as the variable itself
(`$pkg::` is `$pkg`, `@a::` is `@a`, `%h::<k>` is `%h<k>`), and the scalar,
array and hash variable parsers now consume the separator the same way, while
`$::(...)` symbolic lookup keeps its meaning.

Two more gaps followed once the module loaded. A colonpair adverb after a
`.= method(...)` argument list (`%h<k> .= subst('""', '"'):g`) was only bound
to the call when the target was a plain variable; the indexed-target and topic
`.=` forms now share one helper that appends trailing adverbs to the call, as
the postfix loop does for `$s.subst(...):g`. And `|$stash` slipped the Stash as
one opaque item instead of its symbol pairs; a Stash is a `Map`, so it now
slips exactly like `|%h`.

With those, `t/01-over-classes.rakutest` (7 tests) and
`t/02-over-grammars.rakutest` (5 tests) both pass under mutsu.
