use Test;

# The shared `+`, `-` and `*` routines answer a machine-word Int pair first
# (#10111). The forms that reach them without the VM opcode's own fast path
# (a reduction, a metaop, a `callsame` into the core candidate) must give
# the same answers, widening to a BigInt on overflow.

plan 9;

my $max = 9223372036854775807;
my $min = -9223372036854775808;

is-deeply ([+] 1, 2, 3), 6, 'reduce +';
is-deeply ([+] $max, 1), 9223372036854775808, 'reduce + widens on overflow';
is-deeply ([-] 10, 3, 2), 5, 'reduce -';
is-deeply ([-] $min, 1), -9223372036854775809, 'reduce - widens on overflow';
is-deeply ([*] 2, 3, 7), 42, 'reduce *';
is-deeply ([*] $max, 2), 18446744073709551614, 'reduce * widens on overflow';
is-deeply (1, 2, 3 Z* 4, 5, 6), (4, 10, 18), 'zip metaop';

{
    my $calls = 0;
    multi infix:<+>(UInt $a, UInt $b --> UInt) { $calls++; callsame() }
    my ($a, $b) = 40, 2;
    is $a + $b, 42, 'callsame into the core candidate';
    is $calls, 1, 'through the user candidate';
}
