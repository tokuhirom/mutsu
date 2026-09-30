use Test;

plan 3;

my %h = a => 1;
is-deeply %h{"a",}, (1,), 'trailing comma in a brace subscript is a slice';
is-deeply %h{("a"),}, (1,), 'parenthesised key with trailing comma';
my %g;
is-deeply %g{["a","b"],}.elems, 1, 'array key with trailing comma yields one element';
