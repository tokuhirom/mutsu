use Test;

# `[*]` on the Seq that `%h.values` returns lists its elements. It answered
# Nil. Found in Data::TypeSystem's `has-homogeneous-type`
# (`$l[*].&{ $_».are.all eqv ... }` with `$l = %h.values`).

plan 3;

my %h = a => 1, b => 2;
my $s = %h.values;
is $s[*].sort.join(','), '1,2', '%h.values[*]';
my $t = %h.values;
is $t.elems, 2, 'elems first';
is $t[*].elems, 2, 'then [*] still lists the values';
