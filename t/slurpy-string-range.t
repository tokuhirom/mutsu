use Test;

plan 7;

# A slurpy `*@a` flattens a string range argument to its sequence, like it
# already does for a numeric range.
sub count(*@a) { @a.elems }

is count("a".."z"), 26, 'slurpy flattens single-char string range';
is count(1..5), 5, 'slurpy flattens numeric range';
is count("aa".."ad"), 4, 'slurpy flattens multi-char string range';
is count(1, 2, 3), 3, 'slurpy keeps plain list';
is count("x"), 1, 'slurpy with one scalar';

sub first-last(*@a) { (@a[0], @a[*-1]) }
is-deeply first-last("a".."e"), ("a", "e"), 'string-range slurpy elements are correct';
is-deeply first-last(3..7), (3, 7), 'numeric-range slurpy elements are correct';
