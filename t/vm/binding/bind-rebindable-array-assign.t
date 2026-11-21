use Test;

# #17585: `@a = ...` from a routine fills the array `@a` already is, even when
# some routine `:=`-rebinds `@a` (which puts its container behind a binding
# cell). A parameter bound to the same array sees the assignment.

plan 10;

my @a = 1, 2, 3;
sub clear-and-count($n) { @a = (); $n.elems }
is clear-and-count(@a), 0, 'a $ parameter bound to @a sees @a = ()';
is @a.elems, 0, '@a is empty afterwards';

@a = 1, 2, 3;
sub refill(@n) { @a = 5, 6; @n.elems }
is refill(@a), 2, 'an @ parameter bound to @a sees the new contents';
is @a.join(','), '5,6', '@a holds the new contents';

@a = 1, 2;
sub same-array($n) { @a = (9,); $n.WHICH eq @a.WHICH }
ok same-array(@a), 'the assignment keeps the array identity';

my %h = a => 1;
sub clear-hash($n) { %h = (); $n.elems }
is clear-hash(%h), 0, 'a $ parameter bound to %h sees %h = ()';
is %h.elems, 0, '%h is empty afterwards';

# The rebinding routine itself still rebinds.
sub rebind-a() { @a := [7] }
sub rebind-h() { %h := { z => 26 } }
rebind-a();
is @a.join(','), '7', 'the := rebind takes effect';
rebind-h();
is %h<z>, 26, 'the hash := rebind takes effect';

# And an assignment after a rebind fills the newly bound array.
my $before = @a;
sub fill-again() { @a = 8, 9 }
fill-again();
is $before.join(','), '8,9', 'an assignment after the rebind fills the bound array';
