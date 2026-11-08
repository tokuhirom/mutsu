use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 4;

# A tree has no parse that set the language revision: lowering reads the
# unit's own `use v6.x`, and restores the calling unit's afterwards.
my $c = Q[use v6.c; my Int:D $x .= new: 42; $x];
throws-like { EVAL($c.AST) }, X::AdHoc,
    message => /'You cannot create an instance of this type (Int:D)'/,
    'v6.c: a declaration with .= calls new on the constrained type';

my $l = Q[use v6.c; my $r = rx/<[a..z]>/; <a A !>.map({ if $r { $_ } else { "X" } }).join];
is EVAL($l.AST), 'aXX', 'v6.c: an rx boolifies against the map topic';

my $d = Q[my $r = rx/<[a..z]>/; <a A !>.map({ if $r { $_ } else { "X" } }).join];
is EVAL($d.AST), 'XXX', 'without it the rx keeps its defining topic';

is EVAL(Q[my Int:D $y .= new: 42; $y].AST), 42, 'the revision of an earlier tree does not leak';
