use Test;

# Reading a multi-dimensional hash subscript `%h{a;b;c}` (the rvalue side).
# Assignment already worked, but the read previously returned Nil because the
# MultiDimIndex read path only handled arrays. Matches raku 6.e semantics: an
# all-scalar-keys read yields the scalar leaf, a missing key reads as `Any`.

plan 12;

my %h;
%h{"a";"b";"c"} = 42;
%h{"a";"b";"d"} = 7;

is %h{"a";"b";"c"}, 42, 'nested 3-level scalar read';
is %h{"a";"b";"d"}, 7, 'sibling nested key read';

# Reading a partial path returns the nested hash.
is-deeply %h{"a";"b"}.keys.sort, ("c", "d").Seq, 'partial path returns nested hash';

# Missing keys read as the Any type object (not Nil).
ok %h{"a";"b";"x"} === Any, 'missing leaf key reads as Any';
ok %h{"a";"x";"c"} === Any, 'missing mid key reads as Any';
ok %h{"x";"y";"z"} === Any, 'missing top key reads as Any';

# Slices: a list of keys at the final dimension.
is-deeply %h{"a";"b";("c","d")}, (42, 7), 'final-dimension key slice';
is-deeply %h{"a";"b";("c","x")}, (42, Any), 'slice with a missing key fills Any';

# `*` reads all values at a level.
is-deeply %h{"a";"b";*}.sort, (7, 42), 'whatever reads all values at a level';

# Single-level still behaves.
my %flat = a => 1, b => 2, c => 3;
is %flat{"b"}, 2, 'single-dimension scalar read unaffected';
is-deeply %flat{"a","c"}, (1, 3), 'single-dimension slice unaffected';

# Deeper nesting.
my %deep;
%deep{"p";"q";"r";"s"} = "leaf";
is %deep{"p";"q";"r";"s"}, "leaf", '4-level nested read';
