use Test;

# Reading a multi-dimensional hash subscript `%h{a;b;c}` (the rvalue side).
# Assignment already worked, but the read previously returned Nil because the
# MultiDimIndex read path only handled arrays. An ASSOCIATIVE multi-dim
# subscript is a slice, so the read hands back a `List` -- one element per
# selected leaf, even when every dimension is a single key. A missing key
# fills that element with `Any`.

plan 14;

my %h;
%h{"a";"b";"c"} = 42;
%h{"a";"b";"d"} = 7;

is %h{"a";"b";"c"}, 42, 'nested 3-level scalar read';
is %h{"a";"b";"d"}, 7, 'sibling nested key read';

# The read is a one-element slice, not the bare leaf.
is %h{"a";"b";"c"}.raku, '(42,)', 'an all-scalar-keys read is a one-element List';
is %h{"a";"b";"c"}.elems, 1, 'and it holds exactly the selected leaf';

# Reading a partial path returns the nested hash (inside that List).
is-deeply %h{"a";"b"}[0].keys.sort, ("c", "d").Seq, 'partial path returns nested hash';

# Missing keys read as the Any type object (not Nil).
ok %h{"a";"b";"x"}[0] === Any, 'missing leaf key reads as Any';
ok %h{"a";"x";"c"}[0] === Any, 'missing mid key reads as Any';
ok %h{"x";"y";"z"}[0] === Any, 'missing top key reads as Any';

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
