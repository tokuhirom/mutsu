use Test;

plan 8;

# `Hash.new(*%)` stores every pair as an entry and never initializes an
# attribute from it, even one the subclass declares (#13182).
class E is Hash { has $.a; }
my $e = E.new(a => 3);
is $e.keys.sort.join(','), 'a', 'a pair naming a declared attribute is a hash entry';
is $e<a>, 3, '... with its value';
ok !$e.a.defined, '... and the attribute is left uninitialized';

class F is Hash { has $.a = 7; has Int $.n; }
my $f = F.new(a => 3, b => 4, n => 5);
is $f.keys.sort.join(','), 'a,b,n', 'every pair is an entry';
ok !$f.a.defined, 'the attribute default does not run (no bless)';
is $f.n.^name, 'Int', 'a typed attribute keeps its type object';

class G is Hash { has $.n; }
my $g = G.new(<x 1 y 2>);
is $g.keys.sort.join(','), 'x,y', 'positional args are still alternating key/value entries';
ok !$g.n.defined, 'an unrelated attribute is untouched';
