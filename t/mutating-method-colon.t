use Test;
plan 4;

# .= with colon-arg syntax
my @a = 1, 2, 3, 4, 5;
@a .= grep: * > 2;
is @a.elems, 3, '.= grep: filters correctly';
is @a[0], 3, '.= grep: first element';
is @a[1], 4, '.= grep: second element';
is @a[2], 5, '.= grep: third element';
