use Test;
use MONKEY-SEE-NO-EVAL;

plan 2;

# The compile of an EVAL'd unit sees the unit's own `use v6.c`: a stored rx
# value boolifies against the map topic there.
my $src = Q[use v6.c; my $l = rx/<[a..z]>/; <a A !>.map({ if $l { $_ } else { "X" } }).join];
is EVAL($src), 'aXX', 'v6.c: rx boolifies against the topic of the map block';
is EVAL($src.subst('use v6.c; ', '')), 'XXX', 'without it the rx keeps its defining topic';
