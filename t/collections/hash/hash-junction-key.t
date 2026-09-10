use v6;
use Test;

# A Junction used as a hash-initializer key threads: the value is stored under
# each of the junction's members, not under the junction's stringification.
# `%( "a"|"b" => 1 )` sets both %h<a> and %h<b> to 1 (Rakudo semantics).
# Regression: mutsu stored the literal key `any(a, b)` (doc-diff hashmap.rakudoc [1]).

plan 12;

# `%( ... )` literal, multiple pairs.
my %a = %( "a"|"b" => 1, c => 2 );
is-deeply %a.keys.sort.List, ('a', 'b', 'c'), 'any-junction key expands in %( )';
is %a<a>, 1, 'first junction member gets the value';
is %a<b>, 1, 'second junction member gets the value';
is %a<c>, 2, 'a normal pair is unaffected';

# Plain list assignment, multiple pairs.
my %m = ("a"|"b" => 1, c => 2);
is-deeply %m.keys.sort.List, ('a', 'b', 'c'), 'any-junction key expands in list assignment';

# Plain list assignment, a single all-junction pair.
my %b = ("x"&"y" => 3);
is-deeply %b.keys.sort.List, ('x', 'y'), 'all-junction key expands (single pair)';
is %b<x>, 3, 'all-junction member x';
is %b<y>, 3, 'all-junction member y';

# A three-member junction built with .any.
my %h = (<p q r>.any => 9);
is-deeply %h.keys.sort.List, ('p', 'q', 'r'), 'three-member junction key expands';
is %h<q>, 9, 'middle junction member';

# Normal hashes are unaffected.
my %n = (a => 1, b => 2);
is-deeply %n.keys.sort.List, ('a', 'b'), 'a normal hash keeps its keys';

# A non-string, non-junction key still stringifies to a single key.
my %t;
%t{3} = 'x';
is-deeply %t.keys.List, ('3',), 'a numeric key stays a single stringified key';
