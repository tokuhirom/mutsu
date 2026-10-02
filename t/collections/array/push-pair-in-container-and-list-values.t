use Test;

# Found via Net::NetRC: Hash.push with Pairs read from `$` variables inside a
# list, and a List value that must be wrapped (not spliced) on a duplicate key.

plan 6;

my $p = (name => 'a');
my $q = (login => 'b');
is-deeply hash.push(($p,)), {:name<a>}, 'list holding a $-variable Pair';
is-deeply hash.push(|($p, $q)), {:name<a>, :login<b>}, 'slipped $-variable Pairs';
my %h; %h.push(($p, $q));
is-deeply %h, {:name<a>, :login<b>}, 'method form with a list of $-variable Pairs';

my %l; %l.push('k' => ('a', 'b')); %l.push('k' => ('c',));
is-deeply %l<k>, [('a', 'b'), ('c',)], 'duplicate key keeps List values whole';
my %i; %i.push('k' => [1, 2]); %i.push('k' => 3);
is-deeply %i<k>, [1, 2, 3], 'a real Array value is still extended';

is (Nil,).Slip.raku, 'slip(Nil,)', 'List.Slip keeps Nil elements';
