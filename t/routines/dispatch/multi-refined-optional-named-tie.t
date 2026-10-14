use Test;

plan 13;

# A refined candidate (`where`, `subset`) carries rakudo's bind-check flag, so
# it ties a named-parameter candidate that has no positional of its own, and
# the candidate declared first wins (Data::Generators' RandomString).

multi named-first(:$a = 1) { "named-only" }
multi named-first(UInt $s = 1, :$a = 1) { "pos-optional" }
is named-first(), "named-only", 'named-only declared first wins f()';

multi refined-first(UInt $s = 1, :$a = 1) { "pos-optional" }
multi refined-first(:$a = 1) { "named-only" }
is refined-first(), "pos-optional", 'refined declared first wins f()';

multi where-first(:$a = 1) { "named-only" }
multi where-first(Int $s where * > 0 = 1, :$a = 1) { "pos-where" }
is where-first(), "named-only", 'a where clause ties as well';

multi bare-named(:$a) { "named-only" }
multi bare-named(UInt $s = 1) { "pos-optional" }
is bare-named(), "named-only", 'the second candidate needs no named parameter';

# The refinement still wins against a candidate with no bind check at all.
multi plain-none() { "none" }
multi plain-none(UInt $s = 1) { "pos-optional" }
is plain-none(), "pos-optional", 'refined optional beats a bare f()';

# An unrefined optional positional still loses to a named parameter.
multi int-named(:$a = 1) { "named-only" }
multi int-named(Int $s = 1) { "pos-optional" }
is int-named(), "named-only", 'unrefined optional loses to a named candidate';

# A call that reaches the positional is unaffected.
is named-first(3), "pos-optional", 'f(3) binds the positional candidate';
is named-first(:a(2)), "named-only", 'f(:a) keeps the first declared';

# Two refined candidates tie and the first declared wins; they are not ambiguous.
multi dup(UInt $x) { "first" }
multi dup(UInt $x) { "second" }
is dup(1), "first", 'duplicate refined candidates are not ambiguous';

multi opt-first(UInt $s, $t?) { "opt" }
multi opt-first(UInt $s) { "noopt" }
is opt-first(5), "opt", 'refined candidates ignore the optional-positional step (opt first)';

multi noopt-first(UInt $s) { "noopt" }
multi noopt-first(UInt $s, $t?) { "opt" }
is noopt-first(5), "noopt", 'refined candidates ignore the optional-positional step (noopt first)';

# Through a capture, as Data::Generators dispatches.
sub via(|c) { named-first(|c) }
is via(), "named-only", 'f(|c) with empty c';
is via(:a(5)), "named-only", 'f(|c) with a named c';

