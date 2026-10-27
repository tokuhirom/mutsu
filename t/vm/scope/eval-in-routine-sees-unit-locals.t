use Test;

# A routine of the unit that EVALs a string reads the unit's lexicals by name,
# so a local that nothing else in the unit names must still hold its latest
# value there (#15048). `use Test` makes the EVAL in this file one of many.

plan 4;

sub ev($c) { EVAL $c }

my $n = 7;
$n = 8;
is ev('$n'), 8, 'a scalar local, latest store';

my %h = a => 1;
%h<b> = 2;
is ev('%h<b>'), 2, 'a hash local';

my @a = 1, 2;
@a.push(3);
is ev('@a.elems'), 3, 'an array local';

my $s = 'x';
$s ~= 'y';
is ev('$s'), 'xy', 'after a ~=';
