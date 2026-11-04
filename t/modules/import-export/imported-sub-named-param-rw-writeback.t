use lib 't/lib';
use Test;

# From the TOML distribution (#16377): an `is rw` argument handed on to a
# routine that takes a named parameter lost the callee's write when the
# routine was imported, and when it was reached a second time.

use RwArgImported;

plan 7;

sub wrap(str $t, $pos is rw) { named-leaf($t, $pos) }
my $p = 0;
is wrap("x", $p), 5, 'imported sub with a named parameter returns';
is $p, 1, "its write reaches the caller through the caller's own rw parameter";

sub wrap1($pos is rw) { named-leaf("x", $pos) }
my $q = 0;
wrap1($q);
is $q, 1, 'one rw parameter';

for 1..3 -> $i {
    my int $n = 0;
    middle("x", $n);
    is $n, 1, "two levels inside the module, call $i" if $i == 1 || $i == 3;
}

my int $d = 0;
named-leaf("x", $d);
is $d, 1, 'a direct call still works';
my int $e = 5;
middle("x", $e);
is $e, 6, 'a non-zero start';
