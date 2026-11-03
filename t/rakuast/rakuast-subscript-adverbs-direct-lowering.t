use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 15;

# The colonpairs after a subscript (`@a[0]:exists`, `%h<k>:delete`, `@a[1,2]:kv`)
# expand into the shapes the compiler executes where the parser built them; the
# executing finalization no longer converts the node to RakuAST and back. The
# behaviour is the same in the program, in an EVAL string and in an EVAL tree.

my @a = 10, 20, 30;
my %h = a => 1, b => 2, c => 3;

ok @a[1]:exists, ':exists';
nok @a[5]:exists, ':exists on a missing index';
ok @a[5]:!exists, ':!exists';
is-deeply %h<a b>:kv, ('a', 1, 'b', 2), ':kv on a slice';
is-deeply @a[0, 2]:p, (0 => 10, 2 => 30), ':p on a slice';
is %h<b>:v, 2, ':v';

my @e = 1, 2, 3;
is @e[1]:delete, 2, ':delete returns the value';
nok @e[1]:exists, 'and removes the element';

my %g = x => 1, y => 2;
is %g<x>:delete, 1, ':delete on a hash';
is-deeply %g.keys.sort.List, ('y',), 'the key is gone';

is-deeply %h{'a', 'b'}:k, ('a', 'b'), ':k with a braced slice';
my $key = 'c';
ok %h{$key}:exists, 'a variable subscript';
ok %h{$key}:exists(1), ':exists(1) with a value';

is EVAL(Q[my %z = a => 1, b => 2; %z<a b>:kv.elems]), 4, 'EVAL string';
is EVAL(Q[my @z = 5, 6, 7; (@z[1]:exists, @z[9]:exists).join(',')].AST), 'True,False', 'EVAL tree';
