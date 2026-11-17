use lib 't/lib';
use Test;

# A `UNIT::` / `MY::` / `OUTER::` read and a package-prefixed `::("Pkg::$x")`
# lookup reach only scopes that lexically enclose them, so they no longer
# latch the process-wide reflective flag (#17998). The scopes they do reach
# must still see every write to the lexicals they name.

plan 11;

my $y = 6;
sub unit-y() { UNIT::<$y> }
sub outer-y() { OUTER::<$y> }
is unit-y(), 6, 'UNIT::<$y> from a sub';
$y = 7;
is unit-y(), 7, 'UNIT::<$y> sees a later write';
is outer-y(), 7, 'OUTER::<$y> from a sub';

sub my-z() { my $z = 3; $z = 4; MY::<$z> }
is my-z(), 4, 'MY::<$z> sees the routine own write';

for ^2 -> $i {
    my $w = $i * 10;
    is MY::<$w>, $i * 10, "MY::<\$w> in a loop body ($i)";
}

sub nested() {
    my $q = 'outer';
    sub inner() { OUTER::<$q> }
    $q = 'changed';
    inner()
}
is nested(), 'changed', 'OUTER:: from a nested sub sees the write';

sub unit-routines() { UNIT::.keys.grep(*.starts-with('&')).List }
ok unit-routines().grep('&unit-y'), 'UNIT::.keys from a sub lists the unit routines';
ok unit-routines().grep('&nested'), '... every one of them';

use PseudoStashUnitReader;
is package-member('$answer'), 42, '::("Pkg::$name") reads a package member';
is package-member('Thing').^name, 'PseudoStashUnitReader::Thing', '... and a package type';
