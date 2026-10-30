use Test;

# GH-15259 (annotations 0.1.0): `%@array` is hash context on an array
# variable, `@%hash` list context on a hash variable.
plan 4;

my @k = <a 1 b 2>;
my %h = a => 1, b => 2;
is-deeply %@k, {a => IntStr.new(1, "1"), b => IntStr.new(2, "2")}, '%@array';
is %@k<a>, 1, 'and subscripts';
is-deeply @%h.sort.map(*.key).List, <a b>, '@%hash';
is 7 % 4, 3, 'the infix % is untouched';
