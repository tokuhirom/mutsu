use Test;

# A conditional yields the container of the branch it selects, so binding a
# scalar to it aliases that element (Crypt::RC4's
# `my $sy := $!y < 0 ?? @!state[*+$!y] !! @!state[$!y]`). The bind used to keep
# only the value, so a later assignment died with "Cannot assign to an
# immutable value".

plan 6;

my @a = 0 .. 4;
my $i = 3;
my $x := $i > 0 ?? @a[1] !! @a[2];
$x = 42;
is-deeply @a, [0, 42, 2, 3, 4], 'assigning through the bind writes the chosen element';

my $y := $i < 0 ?? @a[*-1] !! @a[$i];
($x, $y) = ($y, $x);
is-deeply @a, [0, 3, 2, 42, 4], 'a list assignment swaps the two bound elements';

my uint8 @n = 0 .. 7;
my $p := @n[1];
my $q := $i < 0 ?? @n[*+$i] !! @n[$i];
($p, $q) = ($q, $p);
is-deeply @n.List, (0, 3, 2, 1, 4, 5, 6, 7), 'works on a native array too';

my %h = a => 1, b => 2;
my $h := $i > 5 ?? %h<a> !! %h<b>;
$h = 20;
is %h<b>, 20, 'a hash element is bound the same way';

my $count = 0;
my $z := ($count++ == 0) ?? @a[0] !! @a[4];
$z = 7;
is @a[0], 7, 'only the chosen branch is bound';
is $count, 1, 'the condition runs once';
