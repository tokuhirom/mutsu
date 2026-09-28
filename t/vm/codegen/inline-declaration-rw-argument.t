use Test;

plan 5;

sub set-sigilless(\x) { x = 2 }
set-sigilless(my $a);
is $a, 2, 'sigilless parameter writes through an inline declaration';

sub set-rw($x is rw) { $x = 4 }
set-rw(my $b = 3);
is $b, 4, 'rw parameter writes through an initialized inline declaration';

sub read-only($x) { $x }
is read-only(my $c = 5), 5, 'ordinary parameter reads an inline declaration';
is $c, 5, 'ordinary call preserves the declared binding';

sub nested-call {
    set-sigilless(my $d);
    $d
}
is nested-call(), 2, 'inline declaration in a routine shares its writable cell';
