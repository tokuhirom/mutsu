use Test;

# Hash.STORE / Array.STORE re-initialize the container in place and return it.

plan 7;

my %h = a => 1;
my $alias := %h;
is-deeply %h.STORE((b => 2, c => 3)), {b => 2, c => 3}, 'Hash.STORE returns the hash';
is-deeply %h, {b => 2, c => 3}, 'contents replaced';
is-deeply $alias, {b => 2, c => 3}, 'an alias sees the store';

my %k;
%k.STORE((1, 2, 3, 4), :INITIALIZE);
is-deeply %k, {'1' => 2, '3' => 4}, 'alternating keys and values';
throws-like { %k.STORE((1, 2, 3)) }, X::Hash::Store::OddNumber, 'odd number of elements';

my @a = 1, 2;
@a.STORE((5, 6, 7));
is-deeply @a, [5, 6, 7], 'Array.STORE replaces the elements';

my role R { method STORE(\v) { callsame; self } }
my %m = a => 1;
%m does R;
%m.STORE((z => 26,));
is-deeply %m.keys.List, ('z',), 'a role STORE reaches the native one through callsame';
